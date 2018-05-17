extern crate wasm_binary;
extern crate wasm_interp;
extern crate stderrlog;
use std::collections::HashMap;

fn main() {
    stderrlog::new()
        .verbosity(4)
        .init()
        .unwrap();

    let mut f = std::fs::File::open(
        std::env::args_os().nth(1)
            .expect("need a file to open"))
        .expect("file open error");

    let module = wasm_binary::Module::read(&mut f).unwrap_or_else(|(e, offset)| {
        eprintln!("Error at {:#x}: {:?}", offset, e);
        std::process::exit(1);
    });

    println!("{:#?}", module);
    println!();

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
        println!();
    }

    if let Some(s) = module.custom_sections.iter().find(|s| s.name == "name") {
        println!("debugging symbols:");
        let decoded = wasm_binary::name_section::SymbolTable::from_bytes(&s.payload)
            .unwrap_or_else(|e| {
                eprintln!("malformed name section: {:?}", e);
                std::process::exit(1);
            });
        println!("{:#?}", decoded);
    } else {
        println!("no debugging symbols");
    }

    println!("\ninstantiating module");
    std::io::Seek::seek(&mut f, std::io::SeekFrom::Start(0)).unwrap();

    // This host environment is for the file `hello.wasm`. For now, it's just debugging stubs that
    // don't even return the right type.
    let mut functions = HashMap::new();
    for f in &["putc_js", "__syscall0", "__syscall1", "__syscall3", "__syscall4", "__syscall5"] {
        functions.insert(f.to_string(), wasm_interp::make_import_function(move |args| {
            println!("{}: {:?}", f, args);
            None
        }));
    }
    let env = wasm_interp::HostEnvironment {
        functions,
    };
    let mut module_env = wasm_interp::instantiate_module(f, env).unwrap();
    println!("\n{:#?}", module_env);

    println!("running the thing");
    let result = module_env.call_function("main", &[]).unwrap();
    println!("result = {:?}", result);
}
