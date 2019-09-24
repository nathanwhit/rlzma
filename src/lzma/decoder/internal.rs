use super::*;

#[derive(Debug)]
pub(crate) struct LZMAOutWindow {
    buf: Vec<Byte>,
    pub(super) pos: u32,
    size: u32,
    pub total_pos: usize,
    pub outstream: LZMAOutputStream,
}

impl Display for LZMAOutWindow {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LZMAOutWindow {{\n\tbuf[pos]: {},\n\tpos: {},\n\t size: {},\n\ttotal_pos: {}\n}}", self.buf[self.pos as usize], self.pos, self.size, self.total_pos)
    }
}

impl LZMAOutWindow {
    pub fn new(out_file: File, dict_size: u32) -> LZMAOutWindow {
        let size = dict_size;
        let buf = vec![0u8; dict_size as usize];
        let pos = 0;
        let total_pos = 0;
        let outstream = LZMAOutputStream::new(out_file);
        LZMAOutWindow {
            buf,
            pos,
            size,
            total_pos,
            outstream
        }
    }
    fn is_full(&self) -> bool {
        self.pos == self.size
    }
    pub(crate) fn put_byte(&mut self, b: Byte) -> Result<()> {
        self.total_pos += 1;
        if self.is_full() {
            self.pos = 0;
            }
        self.buf[self.pos as usize] = b;
        self.pos += 1;
        self.outstream.write_byte(b)?;
        Ok(())
    }
    pub(crate) fn get_byte(&self, dist: u32) -> Result<&Byte> {
        let idx = if dist <= self.pos {
            self.pos - dist
        } else {
            self.size - dist + self.pos
        };
        self.buf.get(idx as usize)
            .ok_or_else(|| format!("The index {} was larger than the output buffer's size: {}", idx, self.size).into())
    }
    pub(crate) fn copy_match(&mut self, dist: u32, len: usize) -> Result<()>{
        for _ in 0..len {
            self.put_byte(*self.get_byte(dist)?)?
        }
        Ok(())
    }
    pub(crate) fn check_distance(&self, dist: u32) -> bool {
        dist <= self.total_pos as u32 || self.is_full()
    }
    pub(crate) fn is_empty(&self) -> bool {
        self.pos == 0 && !self.is_full()
    }
}

#[derive(Debug)]
pub(crate) struct LZMAOutputStream(File);

#[derive(Debug)]
pub(crate) struct LZMAInputStream(Bytes<BufReader<File>>);

impl LZMAInputStream {
    pub fn read_byte(&mut self) -> Result<Byte> {
        self.0.next().ok_or_else(|| ErrorKind::NotEnoughInput(String::from("more data in the input stream")))?.map_err(|e| Error::with_chain(e, "failed to read from input buffer"))
    }
    pub fn new(input_file: File) -> LZMAInputStream {
        LZMAInputStream(
            BufReader::new(input_file).bytes(),
        )
    }
}

impl LZMAOutputStream {
    pub fn write_byte(&mut self, b: Byte) -> Result<()>{
        let len_written = self.0.write(&[b])?;
        ensure!(len_written==1, ErrorKind::WriteFailed);
        Ok(())
    }
    pub fn new(out_file: File) -> LZMAOutputStream {
        LZMAOutputStream(out_file)
    }
}

#[derive(Debug)]
pub(crate) struct LZMARangeDecoder {
    range: u32,
    code: u32,
    instream: LZMAInputStream,
    corrupted: bool,
}

impl LZMARangeDecoder {
    pub fn new(input_file: File) -> LZMARangeDecoder {
        LZMARangeDecoder {
            range: 0xFFFF_FFFF,
            code: 0,
            instream: LZMAInputStream::new(input_file),
            corrupted: false,
        }
    }

    pub fn init(&mut self) -> Result<()> {
        let b = self.instream.read_byte()?;

        for _ in 0..4 {
            self.code = (self.code << 8) | u32::from(self.instream.read_byte()?);
        }
        if b != 0 || self.code == self.range {
            self.corrupted = true;
        }
        if self.corrupted {
            bail!(ErrorKind::LZMAStreamCorrupted)
        } else {
            Ok(())
        }
    }

    pub fn is_finished(&self) -> bool {
        self.code == 0
    }

    const TOP_VALUE: u32 = 1 << 24;

    fn normalize(&mut self) -> Result<()>{
        if self.range < Self::TOP_VALUE {
            self.range <<= 8;
            self.code = (self.code << 8) | u32::from(self.instream.read_byte()?)
        }
        Ok(())
    }
    
    pub fn decode_direct_bits(&mut self, num_bits: usize) -> Result<u32> {
        let mut res: u32 = 0;
        let mut num_bits = num_bits;
        loop {
            self.range >>= 1;
            self.code = self.code.overflowing_sub(self.range).0;
            let t = 0u32.overflowing_sub(self.code >> 31).0;
            // self.code = self.code.overflowing_add(self.range & t).0;
            self.code = self.code.overflowing_add(self.range & t).0;

            // info!("Code: {}", self.code);

            if self.code == self.range {
                self.corrupted = true;
            }

            self.normalize().chain_err(|| "Failed to decode direct bits")?;
            res <<= 1;
            res = res.overflowing_add(t.overflowing_add(1).0).0;
            if num_bits <= 1 {
                break;
            } else {
                num_bits-=1;
            }
        }
        Ok(res)
    }

    pub fn decode_bit(&mut self, prob: &mut Cell<LZMAProb>) -> Result<u32> {
        let mut val = prob.get();
        let bound = (self.range >> NUM_BIT_MODEL_TOTAL_BITS) * u32::from(val);
        let symbol =
            if self.code < bound {
                val += ((1 << NUM_BIT_MODEL_TOTAL_BITS) - val) >> NUM_MOVE_BITS;
                self.range = bound;
                0
            } else {
                val -= val >> NUM_MOVE_BITS;
                self.code -= bound;
                self.range -= bound;
                1
            };
        prob.set(val);
        self.normalize().chain_err(|| "Range decoder failed to decode bit")?;
        Ok(symbol)
    }
}

#[derive(Clone)]
pub(crate) struct LZMABitTreeDecoder {
    num_bits: usize,
    probs: SmallVec<[Cell<LZMAProb>; 256]>
}

impl Display for LZMABitTreeDecoder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LZMABitTreeDecoder {{ num_bits: {} }}", self.num_bits)
    }
}

impl Debug for LZMABitTreeDecoder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <Self as Display>::fmt(&self, f)
    }
}

impl LZMABitTreeDecoder {
    pub fn new(num_bits: usize) -> Self {
        LZMABitTreeDecoder {
            num_bits,
            probs: smallvec![Cell::new(PROB_INIT_VAL); 1 << num_bits as usize]
        }
    }

    pub fn decode(&mut self, range_dec: &mut LZMARangeDecoder) -> Result<usize> {
        let mut m: u32 = 1;
        for _ in 0..self.num_bits {
            m = (m << 1) + range_dec.decode_bit(&mut self.probs[m as usize]).chain_err(|| "Bit tree decoding failed")?;
        }
        Ok((m - (1 << self.num_bits)).try_into()?)
    }

    pub fn reverse_decode(&mut self, range_dec: &mut LZMARangeDecoder) -> Result<usize> {
        LZMABitTreeDecoder::rev_decode(&mut self.probs[..], self.num_bits, range_dec)
    }

    pub fn rev_decode(probs: &mut [Cell<LZMAProb>], num_bits: usize, range_dec: &mut LZMARangeDecoder) -> Result<usize> {
        let mut m: usize = 1;
        let mut symbol = 0;
        for i in 0..num_bits {
            let bit = range_dec.decode_bit(&mut probs[m])?;
            m <<= 1;
            m += bit as usize;
            symbol |= bit << i;
        }
        Ok(symbol.try_into()?)
    }
}

pub(crate) const NUM_POS_BITS_MAX: usize = 4;

#[derive(Clone, Debug)]
pub(crate) struct LZMALenDecoder {
    choice: Cell<LZMAProb>,
    choice_2: Cell<LZMAProb>,
    low_coder: SmallVec<[LZMABitTreeDecoder; (1 << NUM_POS_BITS_MAX)]>,
    mid_coder: SmallVec<[LZMABitTreeDecoder; (1 << NUM_POS_BITS_MAX)]>,
    high_coder: LZMABitTreeDecoder
}

impl Display for LZMALenDecoder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LZMALenDecoder {{ choice: {}, choice_2: {}, low_coder: {:?}, mid_coder: {:?}, high_coder: {:?} }}", self.choice.get(), self.choice_2.get(), self.low_coder, self.mid_coder, self.high_coder)
    }
}

impl LZMALenDecoder {
    pub fn new() -> Self {
        LZMALenDecoder {
            choice: Cell::new(PROB_INIT_VAL),
            choice_2: Cell::new(PROB_INIT_VAL),
            low_coder: smallvec![LZMABitTreeDecoder::new(3); 1 << NUM_POS_BITS_MAX],
            mid_coder: smallvec![LZMABitTreeDecoder::new(3); 1 << NUM_POS_BITS_MAX],
            high_coder: LZMABitTreeDecoder::new(8),
        }
    }
    pub fn decode(&mut self, range_dec: &mut LZMARangeDecoder, pos_state: usize) -> Result<usize> {
        Ok(if range_dec.decode_bit(&mut self.choice)? == 0 {
            self.low_coder[pos_state].decode(range_dec).chain_err(|| format!("Len decoding failed at position state: {}", pos_state))?
        } else if range_dec.decode_bit(&mut self.choice_2)? == 0 {
            8 + self.mid_coder[pos_state].decode(range_dec)?
        } else {
            16 + self.high_coder.decode(range_dec)?
        })
    }
}

#[derive(Clone)]
pub(crate) struct LZMADistanceDecoder {
    pos_slot_dec: SmallVec<[LZMABitTreeDecoder; Self::NUM_LEN_POS_STATES]>,
    pos_decs: SmallVec<[Cell<LZMAProb>; 128]>,
    align_dec: LZMABitTreeDecoder, 
}

impl Display for LZMADistanceDecoder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LZMADistanceDecoder {{ \n\tpos_slot_dec: {:?}, align_dec: {}", self.pos_slot_dec, self.align_dec)
    }
}

impl Debug for LZMADistanceDecoder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        <Self as Display>::fmt(&self, f)
    }
}

impl LZMADistanceDecoder {
    const END_POS_MODEL_IDX: usize = 14;
    const NUM_FULL_DISTS: usize = 1 << (Self::END_POS_MODEL_IDX >> 1);
    const NUM_ALIGN_BITS: usize = 4;
    pub const NUM_LEN_POS_STATES: usize = 4;
    pub fn new() -> Self {
        LZMADistanceDecoder {
            pos_slot_dec: smallvec![LZMABitTreeDecoder::new(6); Self::NUM_LEN_POS_STATES],
            pos_decs: smallvec![Cell::new(PROB_INIT_VAL); 1 + Self::NUM_FULL_DISTS - Self::END_POS_MODEL_IDX],
            align_dec: LZMABitTreeDecoder::new(Self::NUM_ALIGN_BITS),
        }
    }

    pub fn decode_distance(&mut self, len: usize, range_dec: &mut LZMARangeDecoder) -> Result<usize> {
        let mut len_state = len;
        if len_state > Self::NUM_LEN_POS_STATES - 1{
            len_state = Self::NUM_LEN_POS_STATES-1;
        }

        let pos_slot = self.pos_slot_dec[len_state].decode(range_dec)?;
        if pos_slot < 4 {
            return Ok(pos_slot as usize);
        }
        let num_direct_bits = (pos_slot >> 1) - 1;
        let mut dist: u32 = ((2 | (pos_slot & 1)) << num_direct_bits).try_into()?;
        if pos_slot < Self::END_POS_MODEL_IDX.try_into()? {
            dist += LZMABitTreeDecoder::rev_decode(&mut self.pos_decs[..], num_direct_bits as usize, range_dec)? as u32;
        } else {
            dist += range_dec.decode_direct_bits(num_direct_bits-Self::NUM_ALIGN_BITS).chain_err(|| "Failed to decode distance")? << Self::NUM_ALIGN_BITS;
            dist += self.align_dec.reverse_decode(range_dec)? as u32;
        }
        Ok(dist.try_into()?)
    }
}