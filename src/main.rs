#![allow(dead_code)]
#![feature(drain_filter)]

mod data;
mod syntax;

use parsem::{Location, Parsable, Scannable, Token};
use syntax::TokenType;

fn main() {
    let code = std::fs::read("example.minic").unwrap();
    let src = String::from_utf8(code).unwrap();
    let mut index = 0;
    let mut tokens = Vec::new();
    while index < src.len() {
        if let Some((tt, length)) = syntax::TokenType::scan(&src[index..]) {
            tokens.push(Token {
                type_: tt,
                text: src[index..index + length].to_owned(),
                location: Location {
                    file: "example.file".to_owned(),
                    offset: index,
                    length,
                },
            });
            index += length;
        } else {
            panic!("No matching tokens for {}", &src[index..])
        }
    }

    tokens.drain_filter(|t| [TokenType::Whitespace, TokenType::Comment].contains(&t.type_));

    let parsed = syntax::Module::match_parse(&tokens);
    if let Some((module, count)) = parsed.parsed {
        println!("{:#?}", module);
        println!("{:?} {:?}", count, tokens.len());
        assert!(count == tokens.len(), "Unparsed end of output");
    } else {
        println!("{}", parsed.error.unwrap());
    }
}
