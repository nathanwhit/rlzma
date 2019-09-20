#![allow(non_upper_case_globals)]
pub const kNumPosBitsMax: usize = 4;
pub const kNumPosStatesMax: usize = (1 << kNumPosBitsMax);

pub const kLenNumLowBits: usize = 3;
pub const kLenNumLowSymbols: usize = (1 << kLenNumLowBits);
pub const kLenNumHighBits: usize = 8;
pub const kLenNumHighSymbols: usize = (1 << kLenNumHighBits);

pub const LenLow: usize = 0;
pub const LenHigh: usize = (LenLow + 2 * (kNumPosStatesMax << kLenNumLowBits));
pub const kNumLenProbs: usize = (LenHigh + kLenNumHighSymbols);

pub const LenChoice: usize = LenLow;
pub const LenChoice2: usize = (LenLow + (1 << kLenNumLowBits));

pub const kNumStates: u8 = 12;
pub const kNumStates2: u8 = 16;
pub const kNumLitStates: u8 = 7;

pub const kStartPosModelIndex: usize = 4;
pub const kEndPosModelIndex: usize = 14;
pub const kNumFullDistances: usize = (1 << (kEndPosModelIndex >> 1));

pub const kNumPosSlotBits: u8 = 6;
pub const kNumLenToPosStates: u8 = 4;

pub const kNumAlignBits: u8 = 4;
pub const kAlignTableSize: usize = (1 << kNumAlignBits);

pub const kMatchMinLen: u8 = 2;
pub const kMatchSpecLenStart: usize =
    (kMatchMinLen as usize + kLenNumLowSymbols * 2 + kLenNumHighSymbols);

pub const kStartOffset: usize = 1664;
pub const SpecPos: isize = -(kStartOffset as isize);
pub const LZMA_MAX_REQ_INPUT: usize = 20;

pub const IsRep0Long: u16 = (SpecPos + kNumFullDistances as isize) as u16;
pub const RepLenCoder: u16 = (IsRep0Long + (kNumStates2 << kNumPosBitsMax) as u16);
pub const LenCoder: u16 = (RepLenCoder + kNumLenProbs as u16);
pub const IsMatch: u16 = (LenCoder + kNumLenProbs as u16);
pub const Align: u16 = (IsMatch + (kNumStates2 << kNumPosBitsMax) as u16);
pub const IsRep: u16 = (Align + kAlignTableSize as u16);
pub const IsRepG0: u16 = (IsRep + kNumStates as u16);
pub const IsRepG1: u16 = (IsRepG0 + kNumStates as u16);
pub const IsRepG2: u16 = (IsRepG1 + kNumStates as u16);
pub const PosSlot: u16 = (IsRepG2 + kNumStates as u16);
pub const Literal: u16 = (PosSlot + (kNumLenToPosStates << kNumPosSlotBits) as u16);
pub const NUM_BASE_PROBS: usize = (Literal as usize + kStartOffset);
