extern crate lzma_rs;

use lzma_rs::lzma::decoder::{LZMADecoder};
use std::fs::{File};
use std::path::Path;
fn main() {
    let in_path = Path::new("simple.txt.lzma");
    let out_path = Path::new("simple_decoded.txt");
    let out_file = File::create(out_path).unwrap();
    let in_file = File::open(in_path).unwrap();
    let mut decoder = LZMADecoder::new(in_file, out_file);
    decoder.decode().expect("Decoding errored!");
}