pub mod constants;

use alloc::boxed::Box;
use alloc::vec::Vec;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, BufWriter, Read, Write, Bytes};
use smallvec::{SmallVec, smallvec};
use std::cell::Cell;
use std::convert::{TryInto};
use std::fmt::Debug;
use std::mem;
use std::u64;
use error_chain::{bail, ChainedError, ensure};
pub use crate::errors::{Result, Error, ErrorKind, ResultExt};
use std::ops::Not;

type Byte = u8;

#[cfg(prob_u32)]
type LZMAProb = u32;

type LZMAProb = u16;

const MIN_MATCH_LEN: usize = 2;

#[derive(Clone, Debug)]
struct LZMAProps {
    lc: Byte,
    lp: Byte,
    pb: Byte,
    dict_size: u32,
}

impl LZMAProps {
    const DICT_MIN: u32 = 1 << 12;
    pub fn decode_properties(properties: &[Byte]) -> LZMAProps {
        let mut d = properties[0];
        if d >= (9*5*5) {
            panic!("Incorrect LZMA properties!");
        }
        let lc = d % 9;
        d /= 9;
        let pb = d/5;
        let lp = d%5;
        let mut dict_size_in_props: u32 = 0;
        for i in 0..4 {
            dict_size_in_props |= u32::from(properties[i+1]) << (8*i);
        }
        let dict_size = u32::max(dict_size_in_props, Self::DICT_MIN);
        LZMAProps {
            lc,
            lp,
            pb,
            dict_size,
        }
    }
}

#[derive(Debug)]
struct LZMAValueState {
    len: usize,
    rep0: u32,
    rep1: u32,
    rep2: u32,
    rep3: u32,
    unpack_size: usize
}

impl LZMAValueState {
    pub fn new() -> Self {
        LZMAValueState {
            len: 0,
            rep0: 0,
            rep1: 0,
            rep2: 0,
            rep3: 0,
            unpack_size: 0
        }
    }
}

#[derive(Debug)]
pub struct LZMADecoder {
    props: LZMAProps,
    literal_probs: SmallVec<[Cell<LZMAProb>; 2048]>,
    out_window: LZMAOutWindow,
    range_dec: LZMARangeDecoder,
    len_dec: LZMALenDecoder,
    rep_len_dec: LZMALenDecoder,
    dist_dec: LZMADistanceDecoder,
    unpack_size: u64,
}

impl LZMADecoder {
    // fn decode_to_dict(&mut self, _limit: usize) {
    //     let _pb_mask = 1 << (self.props.pb - 1);
    //     let lc = self.props.lc;
    //     let _lp_mask = (0x100 << self.props.lp) - (0x100 >> lc);
    //     let _dict = &mut self.dict;
    //     let _dict_buf_size = self.dict.capacity();
    // }
    pub fn new(mut input_file: File, output_file: File) -> LZMADecoder {
        let mut raw_props: [Byte; 5] = [0; 5];
        input_file.read_exact(&mut raw_props).expect("Failed to read properties from file");
        let props = LZMAProps::decode_properties(&raw_props);
        let mut raw_unpack_size: [Byte; 8] = [0; 8];
        input_file.read_exact(&mut raw_unpack_size).expect("Failed to read uncompressed size from file");
        let unpack_size = u64::from_le_bytes(raw_unpack_size);
        let literal_probs = smallvec![Cell::new(PROB_INIT_VAL); 0x300<<(props.lc + props.lp)];
        let dict_size = props.dict_size;
        LZMADecoder {
            props,
            literal_probs,
            out_window: LZMAOutWindow::new(output_file, dict_size),
            range_dec: LZMARangeDecoder::new(input_file),
            len_dec: LZMALenDecoder::new(),
            rep_len_dec: LZMALenDecoder::new(),
            dist_dec: LZMADistanceDecoder::new(),
            unpack_size
        }
    }

    fn decode_literal(&mut self, state: usize, rep0: u32) -> Result<()> {
        let prev_byte = if self.out_window.is_empty().not() {
            *self.out_window.get_byte(1)?
        } else {
            0
        };
        let mut symbol = 1;
        let lit_state = ((self.out_window.total_pos & ((1 << self.props.lp) - 1)) << self.props.lc) + (prev_byte >> (8 - self.props.lc)) as usize;

        let probs = &mut self.literal_probs[0x300 * lit_state..];
        if state >= 7 {
            let mut match_byte: usize = usize::from(*self.out_window.get_byte(rep0 + 1)?);
            loop {
                let match_bit: usize = (match_byte >> 7) & 1;
                match_byte <<= 1;
    
                let bit: usize = self.range_dec.decode_bit(&mut probs[((1 + match_bit) << 8) as usize + symbol])? as usize;
                symbol = (symbol << 1) | bit;
                if match_bit != bit {
                    break;
                }
                if symbol >= 0x100 {
                    break;
            }
            }
        }

            while symbol < 0x100 {
            symbol = (symbol << 1) | self.range_dec.decode_bit(&mut probs[symbol])? as usize;
            }

            self.out_window.put_byte((symbol - 0x100) as Byte).chain_err(|| "Decode of literal data failed!")?;
        Ok(())
    }

    const NUM_STATES: usize = 12;

    pub fn decode(&mut self) -> Result<LZMADecoderRes> {
        self.range_dec.init()?;

        let (need_marker, size_defined) = if self.unpack_size == u64::MAX {
            (true, false)
        } else {
            (true, true)
        };


        let mut is_match = vec![Cell::new(PROB_INIT_VAL); Self::NUM_STATES << NUM_POS_BITS_MAX];
        let mut is_rep = vec![Cell::new(PROB_INIT_VAL); Self::NUM_STATES];
        let mut is_rep_g0 = vec![Cell::new(PROB_INIT_VAL); Self::NUM_STATES];
        let mut is_rep_g1 = vec![Cell::new(PROB_INIT_VAL); Self::NUM_STATES];
        let mut is_rep_g2 = vec![Cell::new(PROB_INIT_VAL); Self::NUM_STATES];
        let mut is_rep0_long = vec![Cell::new(PROB_INIT_VAL); Self::NUM_STATES << NUM_POS_BITS_MAX];

        let (mut rep0, mut rep1, mut rep2, mut rep3) = (0, 0, 0, 0);

        let mut state = 0;

        loop {
            if size_defined && self.unpack_size == 0 
            && !need_marker && self.range_dec.is_finished() {
                return Ok(LZMADecoderRes::FinishedUnmarked);
            }
            let pos_state = self.out_window.total_pos & ((1 << self.props.pb) - 1);
            let state2 = (state << NUM_POS_BITS_MAX) + pos_state;
            match self.decode_bit(&mut is_match[state2])? {
                // Literal
                0 => {
                    if size_defined && self.unpack_size == 0 {
                        // return Err(Error::;
                        bail!(ErrorKind::NotEnoughInput(String::from("literal data")));
                    }
                    self.decode_literal(state, rep0)?;
                    state = LZMADecoder::update_state_literal(state);
                    self.unpack_size-=1;
                }
                1 => { 
                    if size_defined && self.unpack_size == 0 {
                        bail!(ErrorKind::NotEnoughInput(String::from("match encoded data")));
                    }
                    match self.decode_bit(&mut is_rep[state])? {
                        // Simple match
                        0 => {
                            rep3 = rep2;
                            rep2 = rep1;
                            rep1 = rep0;
                            let len = self.len_dec.decode(&mut self.range_dec, pos_state)?;
                            state = LZMADecoder::update_state_match(state);
                            rep0 = self.dist_dec.decode_distance(len.try_into()?, &mut self.range_dec)? as u32;
                            if rep0 == 0xFFFF_FFFF {
                                return if self.range_dec.is_finished() {
                                    Ok(LZMADecoderRes::FinishedMarked)
                                } else {
                                    bail!(ErrorKind::EarlyEndMarker);
                                }
                            }
                            if size_defined && self.unpack_size == 0 {
                                bail!(ErrorKind::NotEnoughInput(String::from("Expected simple match encoded data")));
                            }
                            ensure!(rep0 < self.props.dict_size, ErrorKind::OverDictSize(rep0, self.props.dict_size));
                            ensure!(self.out_window.check_distance(rep0), format!("Distance was too large: {}\nPosition was: {}", rep0, self.out_window.pos));
                            self.copy_match_symbols(len.try_into()?, rep0, size_defined)?;
                        }
                        // Rep match
                        1 => {
                                if size_defined && self.unpack_size == 0 {
                                    bail!(ErrorKind::NotEnoughInput(String::from("repeated match encoded data")))
                                }
                                if self.out_window.is_empty() {
                                    bail!("Output window was empty when decoding a repeated match");
                                }
                                match self.decode_bit(&mut is_rep_g0[state])? {
                                // Rep match distance = rep0
                                0 => match self.decode_bit(&mut is_rep0_long[state2])? {
                                    // Short rep match
                                    0 => {
                                        state = Self::update_state_shortrep(state);
                                        self.out_window.put_byte(*self.out_window.get_byte(rep0 + 1)?)?;
                                        self.unpack_size -= 1;
                                        continue;
                                    }
                                    // Rep match 0
                                    1 => {
                                        let len =self.rep_len_dec.decode(&mut self.range_dec, pos_state)?;
                                        state = Self::update_state_rep(state);
                                        self.copy_match_symbols(len as usize, rep0, size_defined)?;
                                    }
                                    other => Self::unexpected_value(other)?
                                }
                                // Keep matching
                                1 => match self.decode_bit(&mut is_rep_g1[state])? {
                                    // Rep match 1
                                    0 => {
                                        mem::swap(&mut rep1, &mut rep0);
                                        let len =self.rep_len_dec.decode(&mut self.range_dec, pos_state)?;
                                        state = Self::update_state_rep(state);
                                        self.copy_match_symbols(len as usize, rep0, size_defined)?;
                                    }
                                    // Keep matching
                                    1 => match self.decode_bit(&mut is_rep_g2[state])? {
                                        // Rep match 2
                                        0 => {
                                            let dist = rep2;
                                            rep2 = rep1;
                                            rep1 = rep0;
                                            rep0 = dist;
                                            let len =self.rep_len_dec.decode(&mut self.range_dec, pos_state)?;
                                            state = Self::update_state_rep(state);
                                            self.copy_match_symbols(len as usize, rep0, size_defined)?;
                                        }
                                        // Rep match 3
                                        1 => {
                                            let dist = rep3;
                                            rep3 = rep2;
                                            rep2 = rep1;
                                            rep1 = rep0;
                                            rep0 = dist;
                                            let len =self.rep_len_dec.decode(&mut self.range_dec, pos_state)?;
                                            state = Self::update_state_rep(state);
                                            self.copy_match_symbols(len as usize, rep0, size_defined)?;
                                        }
                                        other => Self::unexpected_value(other)?
                                    }
                                    other => Self::unexpected_value(other)?
                                }
                                other => Self::unexpected_value(other)?
                            }
                        }
                        other => Self::unexpected_value(other)?
                    }
                }
                other => Self::unexpected_value(other)?
            }
        }
    }

    fn decode_bit(&mut self, prob: &mut Cell<LZMAProb>) -> Result<u8> {
        let decoded = self.range_dec.decode_bit(prob)?;
        Ok(if decoded == 1 {
            1
        } else if decoded == 0 {
            0
        } else {
            bail!("Range-Decoded bit was an unexpected result (not 0/1)")
        })
    }

    fn copy_match_symbols(&mut self, len: usize, rep0: u32, size_defined: bool) -> Result<()> {
        let mut len = len + MIN_MATCH_LEN;
        let has_error = if size_defined && self.unpack_size < len as u64 {
            len = self.unpack_size.try_into()?;
            true
        } else {
            false
        };
        self.out_window.copy_match(rep0 + 1, len)?;
        self.unpack_size -= len as u64;
        if has_error {
            bail!(ErrorKind::NotEnoughInput(String::from("matched symbols to copy")));
        } else {
            Ok(())
        }
    }

    fn unexpected_value<T: Debug>(val: T) -> Result<()> {
        bail!(format!("Unexpected value found: {:?}", val))
    }

    fn update_state_literal(state: usize) -> usize {
        if state < 4 {
            0
        } else if state < 10 {
            state - 3
        } else {
            state -6
        }
    }

    fn update_state_match(state: usize) -> usize {
        if state < 7 {
            7
        } else {
            10
        }
    }
    fn update_state_rep(state: usize) -> usize {
        if state < 7 {
            8
        } else {
            11
        }
    }
    fn update_state_shortrep(state: usize) -> usize {
        if state < 7 {
            9
        } else {
            11
        }
    }
}

pub enum LZMADecoderRes {
    FinishedUnmarked,
    FinishedMarked
}

#[derive(Debug)]
pub enum LZMADecoderMode {
    MarkedUnsized,
    MarkedSized(usize),
    UnmarkedSized(usize),
}

#[derive(Debug)]
struct LZMAOutWindow {
    buf: Vec<Byte>,
    pos: u32,
    size: u32,
    pub total_pos: usize,
    pub outstream: LZMAOutputStream,
}

impl LZMAOutWindow {
    pub fn new(out_file: File, dict_size: u32) -> LZMAOutWindow {
        let size = dict_size;
        let buf = Vec::with_capacity(dict_size as usize);
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
    fn put_byte(&mut self, b: Byte) -> Result<()> {
        self.total_pos += 1;
        self.buf.push(b);
        self.pos += 1;
        if self.is_full() {
            self.pos = 0;
            unsafe {
                self.buf.set_len(0)
            }
        }
        self.outstream.write_byte(b)?;
        Ok(())
    }
    fn get_byte(&self, dist: u32) -> Result<&Byte> {
        let idx = if dist <= self.pos {
            self.pos - dist
        } else {
            self.size - dist + self.pos
        };
        self.buf.get(idx as usize)
            .ok_or_else(|| format!("The index {} was larger than the output buffer's size: {}", idx, self.size).into())
    }
    fn copy_match(&mut self, dist: u32, len: usize) -> Result<()>{
        for _ in 0..len {
            self.put_byte(*self.get_byte(dist)?)?
        }
        Ok(())
    }
    fn check_distance(&self, dist: u32) -> bool {
        dist <= self.pos || self.is_full()
    }
    fn is_empty(&self) -> bool {
        self.pos == 0 && !self.is_full()
    }
}

#[derive(Debug)]
struct LZMAOutputStream(BufWriter<File>);

#[derive(Debug)]
struct LZMAInputStream(Bytes<BufReader<File>>);

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
        LZMAOutputStream(BufWriter::new(out_file))
    }
}


const NUM_BIT_MODEL_TOTAL_BITS: LZMAProb = 11;
const NUM_MOVE_BITS: LZMAProb = 5;
const PROB_INIT_VAL: LZMAProb = ((1 << NUM_BIT_MODEL_TOTAL_BITS) / 2);

#[derive(Debug)]
struct LZMARangeDecoder {
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
            println!("Code: {}", self.code);
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

#[derive(Clone, Debug)]
struct LZMABitTreeDecoder {
    num_bits: usize,
    probs: SmallVec<[Cell<LZMAProb>; 256]>
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

const NUM_POS_BITS_MAX: usize = 4;

#[derive(Clone, Debug)]
struct LZMALenDecoder {
    choice: Cell<LZMAProb>,
    choice_2: Cell<LZMAProb>,
    low_coder: SmallVec<[LZMABitTreeDecoder; (1 << NUM_POS_BITS_MAX)]>,
    mid_coder: SmallVec<[LZMABitTreeDecoder; (1 << NUM_POS_BITS_MAX)]>,
    high_coder: LZMABitTreeDecoder
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

#[derive(Clone, Debug)]
struct LZMADistanceDecoder {
    pos_slot_dec: SmallVec<[LZMABitTreeDecoder; Self::NUM_LEN_POS_STATES]>,
    pos_decs: SmallVec<[Cell<LZMAProb>; 128]>,
    align_dec: LZMABitTreeDecoder, 
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