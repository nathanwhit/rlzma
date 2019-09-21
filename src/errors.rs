use error_chain::{error_chain};

error_chain! {
    foreign_links {
        Fmt(std::fmt::Error);
        Io(std::io::Error);
        Int(std::num::TryFromIntError);
        Impossible(std::convert::Infallible);
    }

    errors {
        NotEnoughInput(s: String) {
            description("Input LZMA stream ended prematurely"),
            display("Expected {}", s),
        }

        EarlyEndMarker {
            description("End marker appeared before the data was fully decompressed")
        }

        LZMAStreamCorrupted {
            description("The LZMA stream was corrupted during the process of decoding"),
        }

        SymbolCopyFailed {
            description("Failed to copy decoded symbols")
        }

        OverDictSize(l: u32, d: u32) {
            description("A match distance was greater than the encoding dictionary size"),
            display("Found match distance: `{}` but dictionary size is: `{}`", l, d),
        }

        WriteFailed {
            description("Failed to write decoded byte")
        }
    }
}