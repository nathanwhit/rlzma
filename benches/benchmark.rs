use criterion::{criterion_main, criterion_group, Criterion};

extern crate rustlzma;
use rustlzma::lzma::decoder::LZMADecoder;
use std::fs::File;
use std::io::{Seek, SeekFrom};
use std::path::Path;

pub fn benchmark_file_decode(c: &mut Criterion, input: &Path, bench_name: &str) {
    let in_file = File::open(&input).unwrap();
    let out_path = input.with_file_name(".dec_bench.tmp");
    let out_file = File::create(&out_path).unwrap();
    c.bench_function(bench_name, |b| b.iter(|| LZMADecoder::decode(in_file.try_clone().map(|mut f| {f.seek(SeekFrom::Start(0)).unwrap(); f}).unwrap(), out_file.try_clone().map(|mut f| {f.seek(SeekFrom::Start(0)).unwrap(); f}).unwrap()).unwrap()));
    std::fs::remove_file(&out_path).unwrap();
}

pub fn bench_enwik8(c: &mut Criterion) {
    benchmark_file_decode(c, Path::new("benches/bench_files/enwik8.lzma"), "enwik8 decoding");
}

pub fn bench_pi(c: &mut Criterion) {
    benchmark_file_decode(c, Path::new("benches/bench_files/pi.txt.lzma"), "pi decoding");
}

pub fn bench_bible(c: &mut Criterion) {
    benchmark_file_decode(c, Path::new("benches/bench_files/bible.txt.lzma"), "bible decoding");
}
pub fn bench_ecoli(c: &mut Criterion) {
    benchmark_file_decode(c, Path::new("benches/bench_files/E.coli.lzma"), "E.coli decoding");
}
pub fn bench_world192(c: &mut Criterion) {
    benchmark_file_decode(c, Path::new("benches/bench_files/world192.txt.lzma"), "world192 decoding");
}

criterion_group!{
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = bench_enwik8, bench_pi, bench_bible, bench_ecoli, bench_world192
}
criterion_main!(benches);