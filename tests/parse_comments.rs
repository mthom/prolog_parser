extern crate prolog_parser;

use prolog_parser::ast::*;
use prolog_parser::lexer::{Lexer, Token};
use prolog_parser::tabled_rc::TabledData;

use std::rc::Rc;

fn read_all_tokens(text: &str) -> Result<Vec<Token>, ParserError> {
    let atom_tbl = TabledData::new(Rc::new("my_module".to_string()));
    let flags = MachineFlags::default();
    let mut stream = parsing_stream(text.as_bytes())?;
    let mut lexer = Lexer::new(atom_tbl, flags, &mut stream);

    let mut tokens = Vec::new();
    while !lexer.eof()? {
        let token = lexer.next_token()?;
        tokens.push(token);
    }
    Ok(tokens)
}

#[test]
fn empty_multiline_comment() -> Result<(), ParserError> {
    let tokens = read_all_tokens("/**/ 4\n")?;
    assert_eq!(tokens, [Token::Constant(Constant::Fixnum(4))]);
    Ok(())
}

#[test]
fn any_char_multiline_comment() -> Result<(), ParserError> {
    let tokens = read_all_tokens("/* █╗╚═══╝ © */ 4\n")?;
    assert_eq!(tokens, [Token::Constant(Constant::Fixnum(4))]);
    Ok(())
}
