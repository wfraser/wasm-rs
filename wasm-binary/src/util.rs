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

pub fn read_vari32<R: io::Read>(mut r: R) -> Result<i32, Error> {
    // note: limiting to 4 bytes here seems to not work
    match leb128::read::signed(&mut r) {
        Ok(n) => Ok(n as i32),
        Err(leb128::read::Error::IoError(e)) => Err(Error::IO(e)),
        Err(leb128::read::Error::Overflow) => Err(Error::Invalid("overflow in varint32"))
    }
}

pub fn read_vari64<R: io::Read>(mut r: R) -> Result<i64, Error> {
    // note: limiting to 8 bytes here seem to not work
    match leb128::read::signed(&mut r) {
        Ok(n) => Ok(n),
        Err(leb128::read::Error::IoError(e)) => Err(Error::IO(e)),
        Err(leb128::read::Error::Overflow) => Err(Error::Invalid("overflow in varint32"))
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
