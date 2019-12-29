pub(crate) struct LZMARangeEncoder {
    low: u64,
    range: u32,
    cache: u8,
    cache_size: u64,
}

impl LZMARangeEncoder {
    pub(crate) fn new() -> Self {
        LZMARangeEncoder {
            low: 0,
            range: std::u32::MAX,
            cache: 0,
            cache_size: 1
        }
    }
}