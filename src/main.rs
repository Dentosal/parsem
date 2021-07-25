#![allow(dead_code)]
#![feature(drain_filter)]

mod corelib;
mod data;
mod midend;
mod syntax;

use std::collections::HashMap;

use parsem::{Location, Parsable, Scannable, Token};
use syntax::CompileContext;

use crate::data::Value;
use crate::midend::{
    interpreter::{self, StackItem},
    CompileContextScope, CompileToIr,
};
use crate::syntax::TokenType;

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
    if let Some(module) = parsed.parsed {
        println!("{:#?}", module);
        assert!(parsed.token_count == tokens.len(), "Unparsed end of output");

        let root_scope = CompileContextScope::prelude();
        let mut ctx = CompileContext {
            scopes: vec![root_scope],
        };

        let mut functions = HashMap::new();
        for item in module.items {
            let code = item.compile_to_ir(&mut ctx).expect("Compile error");
            println!("# {:?}\n{:#?}", item.name, code);
            functions.insert(item.name.token.text, code);
        }

        let mut interp = interpreter::State::new("main".to_owned());
        interp.run(&functions).expect("Error");
    } else {
        println!("{}", parsed.error.unwrap());
    }
}
