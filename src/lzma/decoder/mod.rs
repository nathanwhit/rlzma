pub mod constants;
pub(crate) mod internal;

pub use std::vec::Vec;
pub use std::io::{BufRead, BufWriter, Read, Write, Bytes};
pub use buf_redux::BufReader;
pub use std::convert::{TryInto};
use std::fmt::Debug;
use std::mem;
use std::u64;
pub use crate::errors::*;
pub use crate::{bail, ensure};
pub use std::{fmt, fmt::Display};
use std::ops::Not;

#[cfg(feature = "debugging")]
pub use std::fs::File;
#[cfg(feature = "debugging")]
pub use log::{info, log, warn, debug};

use internal::*;

pub(crate) type Byte = u8;

#[cfg(prob_u32)]
type LZMAProb = u32;

type LZMAProb = u16;

pub(crate) const MIN_MATCH_LEN: usize = 2;
pub(crate) const NUM_BIT_MODEL_TOTAL_BITS: LZMAProb = 11;
pub(crate) const NUM_MOVE_BITS: LZMAProb = 5;
pub(crate) const PROB_INIT_VAL: LZMAProb = ((1 << NUM_BIT_MODEL_TOTAL_BITS) / 2);

#[derive(Clone, Debug, Copy)]
pub struct LZMAProps {
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
        dict_size_in_props |= u32::from(properties[1]);
        dict_size_in_props |= u32::from(properties[2]) << (8);
        dict_size_in_props |= u32::from(properties[3]) << (16);
        dict_size_in_props |= u32::from(properties[4]) << (24);
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
pub(crate) struct LZMACacher {
    props: LZMAProps,
    tot_pos_mask: usize,
    prev_byte_shift: usize
}

impl LZMACacher {
    pub(crate) fn new(props: LZMAProps) -> Self {
        let prev_byte_shift = 8 - props.lc as usize;
        let tot_pos_mask = (1 << props.lp) - 1;
        LZMACacher {
            props,
            tot_pos_mask,
            prev_byte_shift
        }
    }
    pub(crate) fn tot_pos_mask(&self) -> usize {
        self.tot_pos_mask
    }
    pub(crate) fn prev_byte_shift(&self) -> usize {
        self.prev_byte_shift
    }
}

#[derive(Clone, Copy)]
pub(crate) enum LZMAState {
    Zero,
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Eleven
}

impl LZMAState {
    fn value(self) -> usize {
        use LZMAState::*;
        match self {
            Zero => 0,
            One => 1,
            Two => 2,
            Three => 3,
            Four => 4,
            Five => 5,
            Six => 6,
            Seven => 7,
            Eight => 8,
            Nine => 9,
            Ten => 10,
            Eleven => 11
        }
    }
}

impl LZMAState {
    pub(crate) fn next_literal(self) -> Self {
        use LZMAState::*;
        match self {
            Zero |
            One  |
            Two  |
            Three  => Zero,
            
            Four    => One,
            Five    => Two,
            Six     => Three,
            Seven   => Four,
            Eight   => Five,
            Nine    => Six,

            Ten     => Four,
            Eleven  => Five,
        }
    }
    pub(crate) fn next_match(self) -> Self {
        use LZMAState::*;
        match self {
            Zero |
            One  |
            Two  |
            Three|
            Four |
            Five |
            Six     => Seven,
            _       => Ten,
        }
    }
    pub(crate) fn next_rep(self) -> Self {
        use LZMAState::*;
        match self {
            Zero |
            One  |
            Two  |
            Three|
            Four |
            Five |
            Six     => Eight,
            _       => Eleven,
        }
    }
    pub(crate) fn next_shortrep(self) -> Self {
        use LZMAState::*;
        match self {
            Zero |
            One  |
            Two  |
            Three|
            Four |
            Five |
            Six     => Nine,
            _       => Eleven,
        }
    }
}

pub(crate) enum BitMatch {
    Zero,
    One
}

#[derive(Debug)]
pub struct LZMADecoder<R: Read, W: Write> {
    props: LZMAProps,
    literal_probs: Vec<LZMAProb>,
    out_window: LZMAOutWindow<W>,
    range_dec: LZMARangeDecoder<R>,
    len_dec: LZMALenDecoder,
    rep_len_dec: LZMALenDecoder,
    dist_dec: LZMADistanceDecoder,
    unpack_size: u64,
    cacher: LZMACacher,
}

impl<R: Read, W: Write> fmt::Display for LZMADecoder<R, W> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LZMADecoder {{\n\tprops: {:?},\n\tout_window: {},\n\trange_dec: {:?},\n\tlen_dec: {:?},\n\trep_len_dec: {:?},\n\tdist_dec: {:?},\n\tunpack_size: {}\n}}", self.props, self.out_window, self.range_dec, self.len_dec, self.rep_len_dec, self.dist_dec, self.unpack_size)
    }
}

impl<R: Read, W: Write> LZMADecoder<R, W> {
    pub fn with_props(props: LZMAProps, input: R, output: W) -> LZMADecoder<R, W> {
        LZMADecoder {
            props,
            literal_probs: vec![PROB_INIT_VAL; 0x300<<(props.lc + props.lp)],
            len_dec: LZMALenDecoder::new(),
            range_dec: LZMARangeDecoder::new(input),
            out_window: LZMAOutWindow::new(output, props.dict_size),
            rep_len_dec: LZMALenDecoder::new(),
            dist_dec: LZMADistanceDecoder::new(),
            unpack_size: u64::MAX,
            cacher: LZMACacher::new(props)
        }
    }

    pub fn new(mut input: R, output: W) -> LZMADecoder<R, W> {
        let mut raw_props: [Byte; 5] = [0; 5];
        input.read_exact(&mut raw_props).expect("Failed to read properties from input source");
        let props = LZMAProps::decode_properties(&raw_props);
        let mut raw_unpack_size: [Byte; 8] = [0; 8];
        input.read_exact(&mut raw_unpack_size).expect("Failed to read uncompressed size from input source");
        let unpack_size = u64::from_le_bytes(raw_unpack_size);
        let literal_probs = vec![PROB_INIT_VAL; 0x300<<(props.lc + props.lp)];
        let dict_size = props.dict_size;
        LZMADecoder {
            props,
            literal_probs,
            out_window: LZMAOutWindow::new(output, dict_size),
            range_dec: LZMARangeDecoder::new(input),
            len_dec: LZMALenDecoder::new(),
            rep_len_dec: LZMALenDecoder::new(),
            dist_dec: LZMADistanceDecoder::new(),
            unpack_size,
            cacher: LZMACacher::new(props),
        }
    }

    fn tot_pos_mask(&self) -> usize {
        self.cacher.tot_pos_mask()
    }

    fn prev_byte_shift(&self) -> usize {
        self.cacher.prev_byte_shift()
    }
    
    pub fn decode(input: R, output: W) -> Result<()> {
        let mut decoder = LZMADecoder::new(input, output);
        decoder._decode().map_err(|e| {
            eprintln!("{}", e.to_string());
            #[cfg(feature = "debugging")]
            eprintln!("Wrote state at error time to : {}", decoder.dump_state().unwrap());
            e
        })?;
        Ok(())
    }

    fn decode_literal(&mut self, state: usize, rep0: u32) -> Result<()> {
        #[cfg(feature = "debugging")]
        debug!("decoding literal");

        let prev_byte = if self.out_window.is_empty().not() {
            self.out_window.get_byte(1)
        } else {
            0
        };
        let mut symbol = 1;
        let lit_state = ((self.out_window.total_pos & self.tot_pos_mask()) << self.props.lc) + (prev_byte >> self.prev_byte_shift()) as usize;

        let probs = &mut self.literal_probs[0x300 * lit_state..];
        if state >= 7 {
            let mut match_byte: usize = usize::from(self.out_window.get_byte(rep0 + 1));
            loop {
                let match_bit: usize = (match_byte >> 7) & 1;
                match_byte <<= 1;
                let idx = ((1 + match_bit) << 8) as usize + symbol;
                let (bit, prob) = self.range_dec.decode_bit(probs[idx])?;
                probs[idx] = prob;
                let bit = bit as usize;
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
                let (bit, prob) = self.range_dec.decode_bit(probs[symbol])?;
                probs[symbol] = prob;
                symbol = (symbol << 1) | bit as usize;
            }

            self.out_window.put_byte((symbol - 0x100) as Byte);

            #[cfg(feature = "debugging")]
            debug!("literal was : {}", symbol-0x100);

        Ok(())
    }

    const NUM_STATES: usize = 12;

    fn _decode(&mut self) -> Result<LZMADecoderRes> {
        self.range_dec.init()?;

        let (need_marker, size_defined) = if self.unpack_size == u64::MAX {
            (true, false)
        } else {
            (true, true)
        };


        let mut is_match = vec![PROB_INIT_VAL; Self::NUM_STATES << NUM_POS_BITS_MAX];
        let mut is_rep = vec![PROB_INIT_VAL; Self::NUM_STATES];
        let mut is_rep_g0 = vec![PROB_INIT_VAL; Self::NUM_STATES];
        let mut is_rep_g1 = vec![PROB_INIT_VAL; Self::NUM_STATES];
        let mut is_rep_g2 = vec![PROB_INIT_VAL; Self::NUM_STATES];
        let mut is_rep0_long = vec![PROB_INIT_VAL; Self::NUM_STATES << NUM_POS_BITS_MAX];

        let (mut rep0, mut rep1, mut rep2, mut rep3) = (0, 0, 0, 0);
        let pos_state_mask = (1 << self.props.pb) - 1;
        let dict_size = self.props.dict_size;
        let mut state = LZMAState::Zero;

        loop {
            if size_defined && self.unpack_size == 0 
            && !need_marker && self.range_dec.is_finished() {
                return Ok(LZMADecoderRes::FinishedUnmarked);
            }
            let pos_state = self.out_window.total_pos & pos_state_mask;
            let state2: usize = (state.value() << NUM_POS_BITS_MAX) + pos_state;
            match self.decode_bit(&mut is_match[state2])? {
                // Literal
                BitMatch::Zero => {
                    if size_defined && self.unpack_size == 0 {
                        bail!(LZMAError::NotEnoughInput(String::from("literal data")));
                    }
                    self.decode_literal(state.value(), rep0)?;
                    state = state.next_literal();
                    self.unpack_size-=1;
                }
                BitMatch::One => { 
                    if size_defined && self.unpack_size == 0 {
                        bail!(LZMAError::NotEnoughInput(String::from("match encoded data")));
                    }
                    match self.decode_bit(&mut is_rep[state.value()])? {
                        // Simple match
                        BitMatch::Zero => {
                            #[cfg(feature = "debugging")]
                            debug!("decoding simple match");

                            rep3 = rep2;
                            rep2 = rep1;
                            rep1 = rep0;
                            let len = self.len_dec.decode(&mut self.range_dec, pos_state)?;
                            state = state.next_match();
                            rep0 = self.dist_dec.decode_distance(len, &mut self.range_dec)?;
                            if rep0 == 0xFFFF_FFFF {
                                return if self.range_dec.is_finished() {
                                    Ok(LZMADecoderRes::FinishedMarked)
                                } else {
                                    bail!(LZMAError::EarlyEndMarker);
                                }
                            }
                            if size_defined && self.unpack_size == 0 {
                                bail!(LZMAError::NotEnoughInput(String::from("Expected simple match encoded data")));
                            }
                            ensure!(rep0 < dict_size, LZMAError::OverDictSize(rep0, dict_size));
                            ensure!(self.out_window.check_distance(rep0), LZMAError::Other(format!("Distance was too large: {}\nPosition was: {}", rep0, self.out_window.pos)));
                            self.copy_match_symbols(len, rep0, size_defined);
                        }
                        // Rep match
                        BitMatch::One => {
                                #[cfg(feature = "debugging")]
                                debug!("decoding rep match");

                                if size_defined && self.unpack_size == 0 {
                                    bail!(LZMAError::NotEnoughInput(String::from("repeated match encoded data")))
                                }
                                if self.out_window.is_empty() {
                                    bail!(LZMAError::Other(String::from("Output window was empty when decoding a repeated match")));
                                }
                                match self.decode_bit(&mut is_rep_g0[state.value()])? {
                                // Rep match distance = rep0
                                BitMatch::Zero => match self.decode_bit(&mut is_rep0_long[state2])? {
                                    // Short rep match
                                    BitMatch::Zero => {
                                        #[cfg(feature = "debugging")]
                                        debug!("decoding short rep match");

                                        state = state.next_shortrep();
                                        self.out_window.put_byte(self.out_window.get_byte(rep0 + 1));
                                        self.unpack_size -= 1;
                                        continue;
                                    }
                                    // Rep match 0
                                    BitMatch::One => {
                                        #[cfg(feature = "debugging")]
                                        debug!("decoding rep match 0");

                                        let len =self.rep_len_dec.decode(&mut self.range_dec, pos_state)?;
                                        state = state.next_rep();
                                        self.copy_match_symbols(len, rep0, size_defined);
                                    }
                                }
                                // Keep matching
                                BitMatch::One => match self.decode_bit(&mut is_rep_g1[state.value()])? {
                                    // Rep match 1
                                    BitMatch::Zero => {
                                        #[cfg(feature = "debugging")]
                                        debug!("decoding rep match 1");

                                        mem::swap(&mut rep1, &mut rep0);
                                        let len =self.rep_len_dec.decode(&mut self.range_dec, pos_state)?;
                                        state = state.next_rep();
                                        self.copy_match_symbols(len, rep0, size_defined);
                                    }
                                    // Keep matching
                                    BitMatch::One => match self.decode_bit(&mut is_rep_g2[state.value()])? {
                                        // Rep match 2
                                        BitMatch::Zero => {
                                            #[cfg(feature = "debugging")]
                                            debug!("decoding rep match 2");

                                            let dist = rep2;
                                            rep2 = rep1;
                                            rep1 = rep0;
                                            rep0 = dist;
                                            let len =self.rep_len_dec.decode(&mut self.range_dec, pos_state)?;
                                            state = state.next_rep();
                                            self.copy_match_symbols(len, rep0, size_defined);
                                        }
                                        // Rep match 3
                                        BitMatch::One => {
                                            #[cfg(feature = "debugging")]
                                            debug!("decoding rep match 3");

                                            let dist = rep3;
                                            rep3 = rep2;
                                            rep2 = rep1;
                                            rep1 = rep0;
                                            rep0 = dist;
                                            let len =self.rep_len_dec.decode(&mut self.range_dec, pos_state)?;
                                            state = state.next_rep();
                                            self.copy_match_symbols(len, rep0, size_defined);
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn decode_bit(&mut self, prob: &mut LZMAProb) -> Result<BitMatch> {
        let (decoded, p) = self.range_dec.decode_bit(*prob)?;
        *prob = p;
        Ok(if decoded == 1 {
            BitMatch::One
        } else if decoded == 0 {
            BitMatch::Zero
        } else {
            bail!(LZMAError::Other(String::from("Range-Decoded bit was an unexpected result (not 0/1)")))
        })
    }

    fn copy_match_symbols(&mut self, len: usize, rep0: u32, size_defined: bool) {
        let len = len + MIN_MATCH_LEN;
        if size_defined && self.unpack_size < len as u64 {
            // bail!(LZMAError::NotEnoughInput(String::from("matched symbols to copy")));
            panic!();
        }
        self.out_window.copy_match(rep0 + 1, len);
        self.unpack_size -= len as u64;
    }

    #[cfg(feature = "debugging")]
    pub fn dump_state(&self) -> Result<String> {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        loop {
            let dump_name = format!("logs/error-{}.log", rng.gen::<u32>());
            let dumpfile_path = Path::new(&dump_name);
            if dumpfile_path.exists().not() {
                let mut dumpfile = File::create(dumpfile_path)?;
                dumpfile.write_all(format!("{}", self).as_bytes())?;
                return Ok(dump_name.to_owned());
            }
        }
        
    }
}

#[derive(Debug)]
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