extern crate wasm_binary;

fn main() {
    let mut f = std::fs::File::open(
        std::env::args_os().nth(1)
            .expect("need a file to open"))
        .expect("file open error");

    let module = wasm_binary::Module::read(&mut f).unwrap_or_else(|(e, offset)| {
        eprintln!("Error at {:#x}: {:?}", offset, e);
        std::process::exit(1);
    });
    println!("{:#?}", module);
}
