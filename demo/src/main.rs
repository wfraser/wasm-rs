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

    for (i, func) in module.code.iter().enumerate() {
        println!("function {} code:", i);
        let n = func.code.len() as u64;
        let mut reader = std::io::Cursor::new(&func.code);
        while reader.position() != n {
            print!("{:#x}/{:#x}: ", reader.position(), n);
            let instr: wasm_binary::instructions::Instruction =
            wasm_binary::module::Read::read(&mut reader)
                .unwrap_or_else(|e| {
                    eprintln!("Error at {:#x}: {:?}", reader.position(), e);
                    std::process::exit(1);
                });
            println!("{:?}", instr);
        }
    }

    if let Some(s) = module.custom_sections.iter().find(|s| s.name == "name") {
        println!("debugging symbols:");
        let decoded = wasm_binary::name_section::NameSection::read(&s.payload)
            .unwrap_or_else(|e| {
                eprintln!("malformed name section: {:?}", e);
                std::process::exit(1);
            });
        println!("{:#?}", decoded);
    }
}
