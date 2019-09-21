extern crate lzma_rs;

use lzma_rs::lzma::decoder::{LZMADecoder};
use std::fs::{File};
use std::path::Path;
use error_chain::quick_main;
use lzma_rs::errors::Result;

quick_main!(run);

fn run() -> Result<()> {
    let in_path = Path::new("simple.txt.lzma");
    let out_path = Path::new("simple_decoded.txt");
    let out_file = File::create(out_path)?;
    let in_file = File::open(in_path)?;
    let mut decoder = LZMADecoder::new(in_file, out_file);
    decoder.decode()?;
    Ok(())
}