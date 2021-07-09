#![allow(dead_code)]

mod data;
mod syntax;

use parsem::Scannable;

fn main() {
    let code = std::fs::read("example.mini.txt").unwrap();
    let src = String::from_utf8(code).unwrap();
    let mut index = 0;
    while index < src.len() {
        if let Some((token, length)) = syntax::TokenText::scan(&src[index..]) {
            println!("T {:?}", token);
            index += length;
        } else {
            panic!("No matching tokens for {}", &src[index..])
        }
    }
}
