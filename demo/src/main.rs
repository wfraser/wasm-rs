extern crate wasm_binary;
extern crate wasm_interp;
extern crate stderrlog;

use std::collections::HashMap;
use std::fs::File;

#[derive(PartialEq)]
enum Mode {
    Module,
    Code,
    Instantiate,
    Run,
}

struct Args {
    mode: Mode,
    file: File,
}

fn parse_args() -> Option<Args> {
    let mut args = std::env::args().skip(1);
    let mode = match args.next().as_ref().map(|s| s.as_str()) {
        Some("--help") | Some("-h") | None => {
            return None;
        }
        Some("module") => Mode::Module,
        Some("code") => Mode::Code,
        Some("instantiate") => Mode::Instantiate,
        Some("run") => Mode::Run,
        Some(other) => {
            eprintln!("unknown mode {:?}", other);
            return None;
        }
    };

    let path = match args.next() {
        Some(p) => p,
        None => {
            eprintln!("error: missing wasm module filename");
            return None;
        }
    };
    let file = match File::open(path) {
        Ok(f) => f,
        Err(e) => {
            eprintln!("failed to open wasm module: {}", e);
            return None;
        }
    };
    Some(Args { mode, file })
}

fn main() {
    stderrlog::new()
        .verbosity(4)
        .init()
        .unwrap();

    let args = parse_args().unwrap_or_else(|| {
        eprintln!("usage: {} <mode> <wasm module path>", std::env::args().next().unwrap());
        eprintln!("  where <mode> is one of: module, code, instantiate, run");
        std::process::exit(2);
    });

    match args.mode {
        Mode::Module | Mode::Code => {
            let module = wasm_binary::Module::read(&args.file).unwrap_or_else(|(e, offset)| {
                eprintln!("Error at {:#x}: {:?}", offset, e);
                std::process::exit(1);
            });

            match args.mode {
                Mode::Module => println!("{:#?}", module),
                Mode::Code => {
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
                }
                _ => unreachable!()
            }
        }

        Mode::Instantiate | Mode::Run => {
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
            let (mut module_env, mut memory) = wasm_interp::instantiate_module(&args.file, env).unwrap();

            match args.mode {
                Mode::Instantiate => println!("{:#?}", module_env),
                Mode::Run => {
                    println!("running the thing");
                    let result = module_env.call_function("main", &[], &mut memory).unwrap();
                    println!("result = {:?}", result);
                }
                _ => unreachable!()
            }
        }
    }
}
