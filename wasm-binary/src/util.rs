use Error;
use std::io;
use leb128;

pub fn read_varu32<R: io::Read>(r: R) -> Result<u32, Error> {
    let mut r2 = r.take(4);
    match leb128::read::unsigned(&mut r2) {
        Ok(n) => Ok(n as u32),
        Err(leb128::read::Error::IoError(e)) => Err(Error::IO(e)),
        Err(leb128::read::Error::Overflow) => Err(Error::Invalid("overflow in varuint32"))
    }
}

pub fn read_varu1<R: io::Read>(r: R) -> Result<bool, Error> {
    match read_varu32(r.take(1)) {
        Ok(0) => Ok(false),
        Ok(1) => Ok(true),
        Ok(_) => Err(Error::Invalid("varuint1 out of range")),
        Err(e) => Err(e),
    }
}

pub fn read_string<R: io::Read>(mut r: R) -> Result<String, Error> {
    use std::io::Read;
    let len = read_varu32(&mut r)?;
    let mut s = String::new();
    r.take(u64::from(len)).read_to_string(&mut s)
        .map_err(Error::IO)?;
    Ok(s)
}