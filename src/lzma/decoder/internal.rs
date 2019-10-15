use super::*;
use buf_redux::policy::MinBuffered;

mod tests;

#[derive(Debug)]
pub(crate) struct LZMAOutWindow<T: Write> {
    buf: Vec<Byte>,
    pub(super) pos: usize,
    size: usize,
    pub total_pos: usize,
    pub outstream: LZMAOutputStream<T>,
}

impl<T: Write> Display for LZMAOutWindow<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LZMAOutWindow {{\n\tbuf[pos]: {},\n\tpos: {},\n\t size: {},\n\ttotal_pos: {}\n}}", self.buf[self.pos as usize], self.pos, self.size, self.total_pos)
    }
}

impl<T: Write> LZMAOutWindow<T> {
    pub fn new(out: T, dict_size: u32) -> LZMAOutWindow<T> {
        let size = dict_size as usize;
        let buf = vec![0u8; dict_size as usize];
        let pos = 0;
        let total_pos = 0;
        let outstream = LZMAOutputStream::new(out);
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
    pub(crate) fn put_byte(&mut self, b: Byte) {
        self.total_pos += 1;
        if self.is_full() {
            self.pos = 0;
            self.outstream.0.write_all(&self.buf).unwrap();
        }
        self.buf[self.pos] = b;
        self.pos += 1;
    }
    pub(crate) fn get_byte(&self, dist: u32) -> Byte {
        let idx = if dist as usize <= self.pos {
            self.pos - dist as usize
        } else {
            self.size - dist as usize + self.pos
        };
        // self.buf.get(idx).ok_or_else(|| String::from("Index was beyond the buffer").into()).map(|b| *b)
        *self.buf.get(idx).unwrap()
    }
    pub(crate) fn copy_match(&mut self, dist: u32, len: usize){
        (0..len).for_each(|_| { self.put_byte(self.get_byte(dist)) });
    }
    pub(crate) fn check_distance(&self, dist: u32) -> bool {
        dist <= self.total_pos as u32 || self.is_full()
    }
    pub(crate) fn is_empty(&self) -> bool {
        self.pos == 0 && !self.is_full()
    }
}

impl<T: Write> std::ops::Drop for LZMAOutWindow<T> {
    fn drop(&mut self) {
        self.outstream.0.write_all(&self.buf[..self.pos]).expect("Failed to write buffer on drop");
    }
}

#[derive(Debug)]
pub(crate) struct LZMAOutputStream<T: Write>(BufWriter<T>);

#[derive(Debug)]
pub(crate) struct LZMAInputStream<R>(BufReader<R, MinBuffered>);

impl<R: Read> LZMAInputStream<R> {
    pub(crate) fn read_byte(&mut self) -> Byte {
        let b = if self.0.buf_len()==0 {
            self.0.fill_buf().unwrap()[0]
        } else {
            self.0.buffer()[0]
        };
        self.0.consume(1);
        b
    }
    pub fn new(input: R) -> LZMAInputStream<R> {
        LZMAInputStream(
            BufReader::new(input).set_policy(MinBuffered(4))
        )
    }
}

impl<T: Write> LZMAOutputStream<T> {
    pub fn new(out: T) -> LZMAOutputStream<T> {
        LZMAOutputStream(BufWriter::new(out))
    }
}

pub(crate) struct LZMARangeDecoder<R: Read> {
    range: u32,
    code: u32,
    instream: LZMAInputStream<R>,
    corrupted: bool,
}

impl<R: Read> Debug for LZMARangeDecoder<R> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "range: {}, code: {}, corrupted: {}", self.range, self.code, self.corrupted)
    }
}

impl<R: Read> LZMARangeDecoder<R> {
    pub fn new(input: R) -> LZMARangeDecoder<R> {
        LZMARangeDecoder {
            range: 0xFFFF_FFFF,
            code: 0,
            instream: LZMAInputStream::new(input),
            corrupted: false,
        }
    }

    pub fn init(&mut self) -> Result<()> {
        let b = self.instream.read_byte();
        self.code = (self.code << 8) | u32::from(self.instream.read_byte());
        self.code = (self.code << 8) | u32::from(self.instream.read_byte());
        self.code = (self.code << 8) | u32::from(self.instream.read_byte());
        self.code = (self.code << 8) | u32::from(self.instream.read_byte());
        if b != 0 || self.code == self.range {
            self.corrupted = true;
        }
        if self.corrupted {
            bail!(LZMAError::StreamCorrupted)
        } else {
            Ok(())
        }
    }

    pub fn is_finished(&self) -> bool {
        self.code == 0
    }

    const TOP_VALUE: u32 = 1 << 24;

    #[inline]
    fn normalize(&mut self) {
        if self.range < Self::TOP_VALUE {
            self.range <<= 8;
            self.code = (self.code << 8) | u32::from(self.instream.read_byte())
        }
    }
    
    pub fn decode_direct_bits(&mut self, mut num_bits: usize) -> Result<u32> {
        let mut res: u32 = 0;
        while num_bits >= 1 {
            self.range >>= 1;
            self.code = self.code.wrapping_sub(self.range);

            let t = 0u32.wrapping_sub(self.code >> 31);
            self.code = self.code.wrapping_add(self.range & t);

            if self.code == self.range {
                self.corrupted = true;
            }
            self.normalize();
            res = (res<<1).wrapping_add(t.wrapping_add(1));
            num_bits -= 1;
        }
        Ok(res)
    }

    const MAXVAL: LZMAProb = 1 << NUM_BIT_MODEL_TOTAL_BITS;

    pub fn decode_bit(&mut self, prob: LZMAProb) -> Result<(u32, LZMAProb)> {
        let mut val = prob;
        let bound = (self.range >> NUM_BIT_MODEL_TOTAL_BITS) * u32::from(val);
        let symbol =
            if self.code < bound {
                val += (Self::MAXVAL - val) >> NUM_MOVE_BITS;
                self.range = bound;
                0
            } else {
                val -= val >> NUM_MOVE_BITS;
                self.code -= bound;
                self.range -= bound;
                1
            };
        self.normalize();
        Ok((symbol, val))
    }
}

#[derive(Clone)]
pub(crate) struct LZMABitTreeDecoder {
    num_bits: usize,
    probs: Vec<LZMAProb>,
    offset: u32
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
            probs: vec![PROB_INIT_VAL; 1 << num_bits as usize],
            offset: 1 << num_bits
        }
    }

    pub fn decode<R: Read>(&mut self, range_dec: &mut LZMARangeDecoder<R>) -> Result<usize> {
        let mut m: u32 = 1;
        for _ in 0..self.num_bits {
            let (bit, prob) = range_dec.decode_bit(self.probs[m as usize])?;
            self.probs[m as usize] = prob;
            m = (m << 1) + bit;
        }
        Ok((m - self.offset).try_into()?)
    }

    pub fn reverse_decode<R: Read>(&mut self, range_dec: &mut LZMARangeDecoder<R>) -> Result<usize> {
        LZMABitTreeDecoder::rev_decode(&mut self.probs[..], self.num_bits, range_dec)
    }

    pub fn rev_decode<R: Read>(probs: &mut [LZMAProb], num_bits: usize, range_dec: &mut LZMARangeDecoder<R>) -> Result<usize> {
        let mut m: usize = 1;
        let mut symbol = 0;
        for i in 0..num_bits {
            let (bit, prob) = range_dec.decode_bit(probs[m])?;
            probs[m] = prob;
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
    choice: LZMAProb,
    choice_2: LZMAProb,
    low_coder: Vec<LZMABitTreeDecoder>,
    mid_coder: Vec<LZMABitTreeDecoder>,
    high_coder: LZMABitTreeDecoder
}

impl Display for LZMALenDecoder {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LZMALenDecoder {{ choice: {}, choice_2: {}, low_coder: {:?}, mid_coder: {:?}, high_coder: {:?} }}", self.choice, self.choice_2, self.low_coder, self.mid_coder, self.high_coder)
    }
}

impl LZMALenDecoder {
    pub fn new() -> Self {
        LZMALenDecoder {
            choice: PROB_INIT_VAL,
            choice_2: PROB_INIT_VAL,
            low_coder: vec![LZMABitTreeDecoder::new(3); 1 << NUM_POS_BITS_MAX],
            mid_coder: vec![LZMABitTreeDecoder::new(3); 1 << NUM_POS_BITS_MAX],
            high_coder: LZMABitTreeDecoder::new(8),
        }
    }
    pub fn decode<R: Read>(&mut self, range_dec: &mut LZMARangeDecoder<R>, pos_state: usize) -> Result<usize> {
        let (bit, prob) = range_dec.decode_bit(self.choice)?;
        self.choice = prob;
        if bit == 0 {
            Ok(self.low_coder[pos_state].decode(range_dec)?)
        } else {
            let (bit, prob) = range_dec.decode_bit(self.choice_2)?;
            self.choice_2 = prob;
            if bit == 0 {
                Ok(8 + self.mid_coder[pos_state].decode(range_dec)?)
            } else {
                Ok(16 + self.high_coder.decode(range_dec)?)
            }
        }
    }
}

#[derive(Clone)]
pub(crate) struct LZMADistanceDecoder {
    pos_slot_dec: Vec<LZMABitTreeDecoder>,
    pos_decs: Vec<LZMAProb>,
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
            pos_slot_dec: vec![LZMABitTreeDecoder::new(6); Self::NUM_LEN_POS_STATES],
            pos_decs: vec![PROB_INIT_VAL; 1 + Self::NUM_FULL_DISTS - Self::END_POS_MODEL_IDX],
            align_dec: LZMABitTreeDecoder::new(Self::NUM_ALIGN_BITS),
        }
    }

    pub fn decode_distance<R: Read>(&mut self, len: usize, range_dec: &mut LZMARangeDecoder<R>) -> Result<u32> {
        let mut len_state = len;
        if len_state > Self::NUM_LEN_POS_STATES - 1{
            len_state = Self::NUM_LEN_POS_STATES-1;
        }

        let pos_slot = self.pos_slot_dec[len_state].decode(range_dec)?;
        if pos_slot < 4 {
            return Ok(pos_slot.try_into()?);
        }
        let num_direct_bits = (pos_slot >> 1) - 1;
        let mut dist: u32 = ((2 | (pos_slot & 1)) << num_direct_bits).try_into()?;
        if pos_slot < Self::END_POS_MODEL_IDX {
            dist += LZMABitTreeDecoder::rev_decode(&mut self.pos_decs[dist.overflowing_sub(pos_slot as u32).0 as usize..], num_direct_bits as usize, range_dec)? as u32;
        } else {
            dist += range_dec.decode_direct_bits(num_direct_bits-Self::NUM_ALIGN_BITS)? << Self::NUM_ALIGN_BITS;
            dist += self.align_dec.reverse_decode(range_dec)? as u32;
        }
        Ok(dist)
    }
}