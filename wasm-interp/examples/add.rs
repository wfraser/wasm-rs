extern crate env_logger;

extern crate wasm_interp;
use wasm_interp::Value;

const WASM: &'static [u8] = &[
    0x00, b'a', b's', b'm',                     // "\0asm"
    0x01, 0x00, 0x00, 0x00,                     // version 1

    0x01, 0x07,                                 // begin type section, 7 bytes
    0x01, 0x60, 0x02, 0x7f, 0x7f, 0x01, 0x7f,   // (i32, i32) -> i32

    0x03, 0x02,                                 // begin function section, 2 bytes
    0x01, 0x00,                                 // type index 0

    0x07, 0x07,                                 // begin export section, 7 bytes
    0x01, 0x03, b'a', b'd', b'd', 0x00, 0x00,   // export function 0 as "add"

    0x0a, 0x09,                                 // begin code section, 9 bytes
    0x01, 0x07, 0x00,                           // begin function body with locals []
    0x20, 0x00,                                 // get_local 0
    0x20, 0x01,                                 // get_local 1
    0x6a,                                       // i32.add
    0x0b,                                       // end

    0x00, 0x1c,                                 // begin custom section, 28 bytes
    0x04, b'n', b'a', b'm', b'e',               // custom section called "name"
    0x01, // functions subsection
        0x06, // length of subsection
        0x01, // count of functions
            0x00, // function index 0
                0x03, b'a', b'd', b'd', // "add"
    0x02, // locals subsection
        0x0d, // length of subsection
        0x01, // count of functions
            0x00, // function index 0
                0x02, // count of locals
                    0x00, // local 0
                        0x03, b'l', b'h', b's', // "lhs"
                    0x01, // local 1
                        0x03, b'r', b'h', b's', // "rhs"
];

fn usage() {
    eprintln!("usage: {} <a> <b>", std::env::args().next().unwrap());
    eprintln!("  adds <a> and <b> using WebAssembly");
}

fn number_from_args(args: &mut impl Iterator<Item = String>) -> i32 {
    if let Some(s) = args.next() {
        match s.parse::<i32>() {
            Ok(n) => return n,
            Err(e) => {
                eprintln!("invalid number: {}", e);
            }
        }
    }
    usage();
    std::process::exit(1);
}

fn main() {
    env_logger::init();

    let mut args = std::env::args().skip(1);
    let a = number_from_args(&mut args);
    let b = number_from_args(&mut args);

    println!("{} + {}", a, b);

    let host_environment = wasm_interp::HostEnvironment::default();

    let (module, mut state) = wasm_interp::instantiate_module(
            std::io::Cursor::new(WASM),
            host_environment)
        .unwrap_or_else(|e| {
            eprintln!("WASM module instantiation failed: {:?}", e);
            std::process::exit(2);
        });
    
    match module.call_function("add", &[Value::I32(a), Value::I32(b)], &mut state) {
        Ok(Some(Value::I32(n))) => {
            println!("= {}", n);
        }
        Ok(Some(other)) => {
            println!("unexpected result type: {:?}", other);
        }
        Ok(None) => {
            println!("unexpected missing result");
        }
        Err(e) => {
            println!("error: {:?}", e);
        }
    }
}
