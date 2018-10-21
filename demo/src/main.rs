extern crate wasm_binary;
extern crate wasm_interp;
extern crate stderrlog;

use wasm_interp::{ModuleEnvironment, MutableState, Value};

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
            // This host environment is for the file `main.wasm` in `helloworld/out`.

            let mut functions = HashMap::new();

            fn putc_js(_module: &ModuleEnvironment, _state: &mut MutableState, args: &[Value])
                -> Option<Value>
            {
                let byte = unwrap_i32(args[0]);
                std::io::Write::write_all(
                    &mut std::io::stdout(),
                    &[byte as u8]).unwrap();
                None
            }

            functions.insert(
                "putc_js".to_owned(),
                wasm_interp::make_import_function(putc_js));

            fn unwrap_i32(v: Value) -> i32 {
                match v {
                    Value::I32(n) => n,
                    _ => panic!("wrong type; expected I32, not {:?}", v)
                }
            }

            fn syscall(
                n: i32,
                module: &ModuleEnvironment,
                state: &mut MutableState,
                args: &[Value],
                ) -> i32
            {
                match n {
                    45 => { // brk
                        0 // ignore, don't care
                    }
                    54 => { // ioctl
                        0 // ignore, don't care
                    }
                    146 => { // writev

                        // we can implement this ourselves:
                        /*
                        let _fd = unwrap_i32(args[0]);
                        let iov_addr = unwrap_i32(args[1]) as usize;
                        let iovcnt = unwrap_i32(args[2]) as usize;

                        #[repr(C)]
                        #[derive(Debug)]
                        struct Iovec {
                            // WASM is a 32-bit machine
                            iov_base: u32, // actually void*
                            iov_len: u32,  // actually size_t
                        }

                        let iovec = unsafe { state.memory.as_ptr().add(iov_addr) }
                            as *const Iovec;

                        let mut cnt = 0i32;
                        for i in 0 .. iovcnt {
                            let iov: &Iovec = unsafe { &*iovec.add(i) };
                            for j in 0 .. iov.iov_len as usize {
                                let byte: u8 = unsafe {
                                    *state.memory.as_ptr()
                                        .add(iov.iov_base as usize)
                                        .add(j)
                                };
                                putc_js(module, state, &[Value::I32(byte as i32)]);
                            }
                            cnt += iov.iov_len as i32;
                        }
                        cnt
                        */

                        // or call the version exported by the module:
                        let result = module.call_function("writev_c", args, state);
                        unwrap_i32(result.unwrap().unwrap())
                    }
                    _ => unimplemented!("syscall {}, args = {:?}", n, args)
                }
            }

            fn syscall_n() -> Box<
                dyn Fn(&ModuleEnvironment, &mut MutableState, &[Value])
                     -> Option<Value>
                >
            {
                wasm_interp::make_import_function(move |module, state, args| {
                    let n = match args[0] {
                        Value::I32(n) => n,
                        _ => panic!(),
                    };
                    Some(Value::I32(syscall(n, module, state, &args[1..])))
                })
            }

            for n in 0 .. 6 {
                functions.insert(format!("__syscall{}", n), syscall_n());
            }

            let env = wasm_interp::HostEnvironment {
                functions,
            };
            let (mut module_env, mut state) = wasm_interp::instantiate_module(&args.file, env).unwrap();

            match args.mode {
                Mode::Instantiate => println!("{:#?}", module_env),
                Mode::Run => {
                    println!("running the thing");
                    let result = module_env.call_function("main", &[], &mut state).unwrap();
                    println!("result = {:?}", result);
                }
                _ => unreachable!()
            }
        }
    }
}
