extern crate rustlzma;

use rustlzma::lzma::decoder::{LZMADecoder};
use std::fs::{File};
use std::path::{Path, PathBuf};
use std::process::exit;
#[cfg(feature = "debugging")]
use env_logger;

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

    match LZMADecoder::decode(in_file, out_file) {
        Ok(..) => (),
        Err(e) => {
            eprintln!("An error occurred: {}", e.to_string())
        }
    }
}