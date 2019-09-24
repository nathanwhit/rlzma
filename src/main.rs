extern crate lzma_rs;

use lzma_rs::lzma::decoder::{LZMADecoder};
use std::fs::{File};
use std::path::Path;
use error_chain::ChainedError;
use std::process::exit;
#[cfg(debugging)]
use env_logger;
// use lzma_rs::errors::Result;

fn main() {
    #[cfg(debugging)]
    env_logger::init();


    let in_path = Path::new("simple.txt.lzma");
    let out_path = Path::new("simple_decoded.txt");
    let out_file = File::create(out_path).expect("Failed to create output file");
    let in_file = File::open(in_path).expect("Failed to open input file");
    let mut decoder = LZMADecoder::new(in_file, out_file);
    decoder.decode().unwrap_or_else(|e| {
        eprintln!("{}", e.display_chain().to_string());
        exit(1);
    });
}