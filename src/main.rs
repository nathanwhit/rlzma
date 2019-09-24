extern crate lzma_rs;

use lzma_rs::lzma::decoder::{LZMADecoder};
use std::fs::{File};
use std::path::{Path, PathBuf};
use error_chain::ChainedError;
use std::process::exit;
#[cfg(feature = "debugging")]
use env_logger;
// use lzma_rs::errors::Result;

fn main() {
    #[cfg(feature = "debugging")]
    env_logger::init();

    let args: Vec<String> = std::env::args().collect();
    if args.len() < 2 {
        eprintln!("Not enough arguments. Exiting");
        exit(1);
    }

    let in_path = Path::new(&args[1]);
    
    let out_pathbuf = if args.len() < 3 {
        if let Some(Some("lzma")) = in_path.extension().map(|s| s.to_str()) {
            if let Some(filename) = in_path.file_stem() {
                in_path.with_file_name(filename)
            } else {
                eprintln!("The file name was invalid");
                exit(1);
            }
        } else {
            in_path.with_extension("decoded")
        }
    } else {
        PathBuf::from(&args[2])
    };
    let out_file = File::create(&out_pathbuf).expect("Failed to create output file");
    let in_file = File::open(in_path).expect("Failed to open input file");
    let mut decoder = LZMADecoder::new(in_file, out_file);
    decoder.decode().unwrap_or_else(|e| {
        eprintln!("{}", e.display_chain().to_string());
        #[cfg(feature = "debugging")]
        eprintln!("Wrote state at error time to : {}", decoder.dump_state().unwrap());

        exit(1);
    });
}