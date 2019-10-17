use buf_redux::policy::MinBuffered;
use std::io::{Write, Read, BufRead};
use std::{fmt, fmt::{Display, Debug}};
pub use buf_redux::{BufReader, BufWriter};
pub(crate) type Byte = u8;

#[derive(Debug)]
pub(crate) struct LZMAOutWindow<T: Write> {
    buf: Vec<Byte>,
    pub(super) pos: usize,
    size: usize,
    pub total_pos: usize,
    pub outstream: LZMAOutputStream<T>,
}

impl<T: Write> Display for LZMAOutWindow<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "LZMAOutWindow {{\n\tbuf[pos]: {},\n\tpos: {},\n\t size: {},\n\ttotal_pos: {}\n}}", self.buf[self.pos as usize], self.pos, self.size, self.total_pos)
    }
}

impl<T: Write> LZMAOutWindow<T> {
    pub fn new(out: T, dict_size: u32) -> LZMAOutWindow<T> {
        let size = dict_size as usize;
        let buf = vec![0u8; dict_size as usize];
        let pos = 0;
        let total_pos = 0;
        let outstream = LZMAOutputStream::new(out);
        LZMAOutWindow {
            buf,
            pos,
            size,
            total_pos,
            outstream
        }
    }
    fn is_full(&self) -> bool {
        self.pos == self.size
    }
    pub(crate) fn put_byte(&mut self, b: Byte) {
        self.total_pos += 1;
        if self.is_full() {
            self.pos = 0;
            self.outstream.0.write_all(&self.buf).unwrap();
        }
        self.buf[self.pos] = b;
        self.pos += 1;
    }
    pub(crate) fn get_byte(&self, dist: u32) -> Byte {
        let idx = if dist as usize <= self.pos {
            self.pos - dist as usize
        } else {
            self.size - dist as usize + self.pos
        };
        // self.buf.get(idx).ok_or_else(|| String::from("Index was beyond the buffer").into()).map(|b| *b)
        *self.buf.get(idx).unwrap()
    }
    pub(crate) fn copy_match(&mut self, dist: u32, len: usize){
        (0..len).for_each(|_| { self.put_byte(self.get_byte(dist)) });
    }
    pub(crate) fn check_distance(&self, dist: u32) -> bool {
        dist <= self.total_pos as u32 || self.is_full()
    }
    pub(crate) fn is_empty(&self) -> bool {
        self.pos == 0 && !self.is_full()
    }
}

impl<T: Write> std::ops::Drop for LZMAOutWindow<T> {
    fn drop(&mut self) {
        self.outstream.0.write_all(&self.buf[..self.pos]).expect("Failed to write buffer on drop");
    }
}

#[derive(Debug)]
pub(crate) struct LZMAOutputStream<T: Write>(BufWriter<T>);

#[derive(Debug)]
pub(crate) struct LZMAInputStream<R>(BufReader<R, MinBuffered>);

impl<R: Read> LZMAInputStream<R> {
    pub(crate) fn read_byte(&mut self) -> Byte {
        let b = if self.0.buf_len()==0 {
            self.0.fill_buf().unwrap()[0]
        } else {
            self.0.buffer()[0]
        };
        self.0.consume(1);
        b
    }
    pub fn new(input: R) -> LZMAInputStream<R> {
        LZMAInputStream(
            BufReader::new(input).set_policy(MinBuffered(4))
        )
    }
}

impl<T: Write> LZMAOutputStream<T> {
    pub fn new(out: T) -> LZMAOutputStream<T> {
        LZMAOutputStream(BufWriter::new(out))
    }
}