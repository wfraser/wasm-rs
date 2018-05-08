extern crate wasm_binary;

use wasm_binary::Read;
use std::fs::File;

fn main() {
    let mut f = File::open(
        std::env::args_os().nth(1)
            .expect("need a file to open"))
        .expect("file open error");

    let module = wasm_binary::Module::read(&mut f).unwrap();
    println!("{:#?}", module);
}
