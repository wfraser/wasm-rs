#[macro_use] extern crate enum_primitive_derive;
extern crate leb128;
extern crate num_traits;

pub mod instructions;
pub mod module;
pub mod name_section;
mod util;

pub use module::Module;
use std::io;

#[derive(Debug)]
pub enum Error {
    Invalid(&'static str),
    IO(io::Error),
}

struct TrackedStream<R> {
    inner: R,
    offset: u64,
}

impl<R: io::Read> io::Read for TrackedStream<R> {
    fn read(&mut self, buf: &mut [u8]) -> Result<usize, io::Error> {
        let result = self.inner.read(buf);
        if let Ok(n) = result {
            self.offset += n as u64;
        }
        result
    }
}

impl<R> TrackedStream<R> {
    fn new(r: R) -> Self {
        Self {
            inner: r,
            offset: 0,
        }
    }
}

impl Module {
    /// Read a WASM module from a stream.
    pub fn read<R: io::Read>(r: R) -> Result<Self, (Error, u64)> {
        // This is a convenience function so that users don't need to import the Read trait,
        // and also it keeps track and reports where in the input stream any error occurs.
        let mut r2 = TrackedStream::new(r);
        module::Read::read(&mut r2)
            .map_err(|e| (e, r2.offset))
    }
}
