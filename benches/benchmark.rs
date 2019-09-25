use criterion::{criterion_main, criterion_group, Criterion};

extern crate rustlzma;
use rustlzma::lzma::decoder::LZMADecoder;
use std::fs::File;
use std::path::Path;

pub fn benchmark_enwik8() {
    let in_file = File::open("enwik8.lzma").unwrap();
    let out_file = File::create(Path::new("temp_enwik8")).unwrap();
    LZMADecoder::decode(in_file, out_file).unwrap();
}

pub fn benchmark_pi() {
    let in_file = File::open("pi.txt.lzma").expect("PI");
    let out_file = File::create(Path::new("temp_pi")).unwrap();
    LZMADecoder::decode(in_file, out_file).expect("Moo");
}

pub fn run_benchmark(c: &mut Criterion) {
    c.bench_function("enwik8 decoding", |b| b.iter(|| benchmark_enwik8()));
    std::fs::remove_file("temp_enwik8").unwrap();
    c.bench_function("pi decoding", |b| b.iter(|| benchmark_pi()));
    std::fs::remove_file("temp_pi").unwrap();

}

criterion_group!{
    name = benches;
    config = Criterion::default().sample_size(10);
    targets = run_benchmark
}
criterion_main!(benches);