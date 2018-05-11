use Error;
use std::io;
use leb128;

pub fn read_varu32<R: io::Read>(r: R) -> Result<u32, Error> {
    let mut r = r.take(5); // ceil(32 / 7)
    match leb128::read::unsigned(&mut r) {
        Ok(n) => Ok(n as u32),
        Err(leb128::read::Error::IoError(e)) => Err(Error::IO(e)),
        Err(leb128::read::Error::Overflow) => Err(Error::Invalid("overflow in varuint32"))
    }
}

pub fn read_vari32<R: io::Read>(r: R) -> Result<i32, Error> {
    let mut r = r.take(5); // ceil(32 / 7)
    match leb128::read::signed(&mut r) {
        Ok(n) => Ok(n as i32),
        Err(leb128::read::Error::IoError(e)) => Err(Error::IO(e)),
        Err(leb128::read::Error::Overflow) => Err(Error::Invalid("overflow in varint32"))
    }
}

pub fn read_vari64<R: io::Read>(r: R) -> Result<i64, Error> {
    let mut r = r.take(10); // ceil(64 / 7)
    match leb128::read::signed(&mut r) {
        Ok(n) => Ok(n),
        Err(leb128::read::Error::IoError(e)) => Err(Error::IO(e)),
        Err(leb128::read::Error::Overflow) => Err(Error::Invalid("overflow in varint32"))
    }
}

pub fn read_varu1<R: io::Read>(mut r: R) -> Result<bool, Error> {
    let mut buf = [0u8];
    r.read_exact(&mut buf).map_err(Error::IO)?;
    match buf[0] {
        0 => Ok(false),
        1 => Ok(true),
        _ => Err(Error::Invalid("varuint1 out of range")),
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

pub fn read_f64<R: io::Read>(mut r: R) -> Result<f64, Error> {
    let mut buf = [0u8; 8];
    r.read_exact(&mut buf).map_err(Error::IO)?;
    let unsigned = (buf[0] as u64)
         | ((buf[1] as u64) << 8)
         | ((buf[2] as u64) << 16)
         | ((buf[3] as u64) << 24)
         | ((buf[4] as u64) << 32)
         | ((buf[5] as u64) << 40)
         | ((buf[6] as u64) << 48)
         | ((buf[7] as u64) << 56);
    Ok(f64::from_bits(unsigned))
}

pub fn read_f32<R: io::Read>(mut r: R) -> Result<f32, Error> {
    let mut buf = [0u8; 4];
    r.read_exact(&mut buf).map_err(Error::IO)?;
    let unsigned = (buf[0] as u32)
         | ((buf[1] as u32) << 8)
         | ((buf[2] as u32) << 16)
         | ((buf[3] as u32) << 24);
    Ok(f32::from_bits(unsigned))
}
