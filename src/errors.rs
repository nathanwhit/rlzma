pub use thiserror::{Error};
pub(crate) type Result<T> = std::result::Result<T, LZMAError>;
use std::convert::From;
use std::num::TryFromIntError;

#[derive(Error, Debug)]
pub enum LZMAError {
    #[error("not enough input, expected {}", .0)]
    NotEnoughInput(String),

    #[error("end of stream marker appeared before the data was fully decompressed")]
    EarlyEndMarker,

    #[error("the LZMA stream entered an invalid state")]
    StreamCorrupted,

    #[error("found match distance `{}` but dict size is `{}`", .0, .1)]
    OverDictSize(u32, u32),

    #[error("integer type conversion failed, usize is not large enough")]
    ConversionFail(TryFromIntError),

    #[error("{}", .0)]
    Other(String)
}

#[macro_export]
macro_rules!  bail {
    ($e: expr) => {
        return Err($e);
    };
}

#[macro_export]
macro_rules! ensure {
    ($c: expr, $e: expr) => {
        if !($c) {
            bail!($e)
        }
    };
}

impl From<TryFromIntError> for LZMAError {
    fn from(error: TryFromIntError) -> Self {
        LZMAError::ConversionFail(error)
    }
}