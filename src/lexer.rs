extern crate lexical;
extern crate num;

use lexical::parse_lossy;
use num::ToPrimitive;
use num::bigint::BigInt;
use ordered_float::*;

use ast::*;
use string_list::*;
use tabled_rc::*;

use std::io::Read;
use std::rc::Rc;

macro_rules! is_not_eof {
    ($c:expr) => (
        match $c {
            Ok(c) => c,
            Err(ParserError::UnexpectedEOF) => return Ok(true),
            Err(e) => return Err(e)
        }
    )
}

macro_rules! consume_chars_with {
    ($token:expr, $e:expr) => {
        loop {
            match $e {
                Ok(Some(c)) => $token.push(c),
                Ok(None) => continue,
                Err(ParserError::UnexpectedChar(_)) => break,
                Err(e) => return Err(e)
            }
        }
    }
}

macro_rules! put_back_n {
    ($self:expr, $token:expr) => {
        while let Some(c) = $token.pop() {
            $self.return_char(c);
        }
    }
}

#[derive(Clone, PartialEq)]
pub enum Token {
    Constant(Constant),
    Var(Rc<Atom>),
    Open,           // '('
    OpenCT,         // '('
    Close,          // ')'
    OpenList,       // '['
    CloseList,      // ']'
    OpenCurly,      // '{'
    CloseCurly,     // '}'
    HeadTailSeparator, // '|'
    Comma,          // ','
    End
}

pub struct Lexer<'a, R: Read> {
    pub(crate) atom_tbl: TabledData<Atom>,
    pub(crate) reader: &'a mut ParsingStream<R>,
    flags: MachineFlags,
    line_num: usize,
}

impl<'a, R: Read> Lexer<'a, R> {
    pub fn new(atom_tbl: TabledData<Atom>, flags: MachineFlags, src: &'a mut ParsingStream<R>) -> Self
    {
        Lexer { atom_tbl, flags, reader: src, line_num: 0 }
    }

    fn return_char(&mut self, c: char) {
        if new_line_char!(c) {
            self.line_num -= 1;
        }

        self.reader.put_back(Ok(c as u8));
    }

    fn skip_char(&mut self) -> Result<char, ParserError> {
        if let Some(Ok(c)) = self.reader.next() {
            if new_line_char!(c as char) {
                self.line_num += 1;
            }

            Ok(c as char)
        } else {
            Err(ParserError::UnexpectedEOF)
        }
    }

    pub fn eof(&mut self) -> Result<bool, ParserError> {
        if self.reader.peek().is_none() {
            return Ok(true);
        }

        let mut c = is_not_eof!(self.lookahead_char());

        while layout_char!(c) {
            self.skip_char()?;

            if self.reader.peek().is_none() {
                return Ok(true);
            }

            c = is_not_eof!(self.lookahead_char());
        }

        Ok(false)
    }

    pub fn lookahead_char(&mut self) -> Result<char, ParserError> {
        match self.reader.peek() {
            Some(&Ok(b)) => Ok(b as char),
            _ => Err(ParserError::UnexpectedEOF)
        }
    }

    fn single_line_comment(&mut self) -> Result<(), ParserError>
    {
        loop {
            if new_line_char!(self.skip_char()?) {
                break;
            }
        }

        Ok(())
    }

    fn bracketed_comment(&mut self) -> Result<bool, ParserError> {
        if comment_1_char!(self.lookahead_char()?) {
            let c = self.skip_char()?;

            if comment_2_char!(self.lookahead_char()?) {
                self.skip_char()?;

                loop {
                    let mut c = self.lookahead_char()?;
                    while prolog_char!(c) && !comment_2_char!(c) {
                        self.skip_char()?;
                        c = self.lookahead_char()?;
                    }

                    if prolog_char!(c) {
                        self.skip_char()?;
                    }

                    c = self.lookahead_char()?;

                    if !(prolog_char!(c) && !comment_1_char!(c)) {
                        break;
                    }
                }

                let c = self.lookahead_char()?;

                if prolog_char!(c) {
                    self.skip_char()?;
                    Ok(true)
                } else {
                    Err(ParserError::NonPrologChar)
                }
            } else {
                self.return_char(c);
                Ok(false)
            }
        } else {
            Ok(false)
        }
    }

    fn get_back_quoted_char(&mut self) -> Result<char, ParserError> {
        if back_quote_char!(self.lookahead_char()?) {
            let c = self.skip_char()?;

            if !back_quote_char!(self.lookahead_char()?) {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c))
            } else {
                self.skip_char()
            }
        } else if single_quote_char!(self.lookahead_char()?) {
            self.skip_char()
        } else {
            self.get_non_quote_char()
        }
    }

    fn get_back_quoted_item(&mut self) -> Result<Option<char>, ParserError> {
        if backslash_char!(self.lookahead_char()?) {
            let c = self.skip_char()?;

            if new_line_char!(self.lookahead_char()?) {
                self.skip_char()?;
                Ok(None)
            } else {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c))
            }
        } else {
            self.get_back_quoted_char().map(Some)
        }
    }

    fn get_back_quoted_string(&mut self) -> Result<String, ParserError> {
        let c = self.lookahead_char()?;

        if back_quote_char!(c) {
            self.skip_char()?;

            let mut token = String::new();
            consume_chars_with!(token, self.get_back_quoted_item());

            if back_quote_char!(self.lookahead_char()?) {
                self.skip_char()?;
                Ok(token)
            } else {
                Err(ParserError::MissingQuote)
            }
        } else {
            Err(ParserError::UnexpectedChar(c))
        }
    }

    fn get_single_quoted_item(&mut self) -> Result<Option<char>, ParserError> {
        if backslash_char!(self.lookahead_char()?) {
            let c = self.skip_char()?;

            if new_line_char!(self.lookahead_char()?) {
                self.skip_char()?;
                return Ok(None);
            } else {
                self.return_char(c);
            }
        }

        self.get_single_quoted_char().map(Some)
    }

    fn get_single_quoted_char(&mut self) -> Result<char, ParserError> {
        let c = self.lookahead_char()?;

        if single_quote_char!(c) {
            self.skip_char()?;

            if !single_quote_char!(self.lookahead_char()?) {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c))
            } else {
                self.skip_char()
            }
        } else if double_quote_char!(c) || back_quote_char!(c) {
            self.skip_char()
        } else {
            self.get_non_quote_char()
        }
    }

    fn get_double_quoted_item(&mut self) -> Result<Option<char>, ParserError> {
        if backslash_char!(self.lookahead_char()?) {
            let c = self.skip_char()?;

            if new_line_char!(self.lookahead_char()?) {
                self.skip_char()?;
                return Ok(None)
            } else {
                self.return_char(c);
            }
        }

        self.get_double_quoted_char().map(Some)
    }

    fn get_double_quoted_char(&mut self) -> Result<char, ParserError> {
        if double_quote_char!(self.lookahead_char()?) {
            let c = self.skip_char()?;

            if !double_quote_char!(self.lookahead_char()?) {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c))
            } else {
                self.skip_char()
            }
        } else if single_quote_char!(self.lookahead_char()?) {
            self.skip_char()
        } else if back_quote_char!(self.lookahead_char()?) {
            self.skip_char()
        } else {
            self.get_non_quote_char()
        }
    }

    fn get_meta_escape_sequence(&mut self) -> Result<char, ParserError> {
        if backslash_char!(self.lookahead_char()?) {
            let c = self.skip_char()?;

            if meta_char!(self.lookahead_char()?) {
                self.skip_char()
            } else {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c))
            }
        } else {
            Err(ParserError::UnexpectedChar(self.lookahead_char()?))
        }
    }

    fn get_control_escape_sequence(&mut self) -> Result<char, ParserError>
    {
        if backslash_char!(self.lookahead_char()?) {
            let c = self.skip_char()?;

            if symbolic_control_char!(self.lookahead_char()?) {
                match self.skip_char()? {
                    'a' => Ok('\u{07}'), // UTF-8 alert
                    'b' => Ok('\u{08}'), // UTF-8 backspace
                    'v' => Ok('\u{0b}'), // UTF-8 vertical tab
                    'f' => Ok('\u{0c}'), // UTF-8 form feed
                    't' => Ok('\t'),
                    'n' => Ok('\n'),
                    'r' => Ok('\r'),
                    c   => Err(ParserError::UnexpectedChar(c))
                }
            } else {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c))
            }
        } else {
            Err(ParserError::UnexpectedChar(self.lookahead_char()?))
        }
    }

    fn get_octal_escape_sequence(&mut self) -> Result<char, ParserError>
    {
        if backslash_char!(self.lookahead_char()?) {
            let c = self.skip_char()?;
            let mut lac = self.lookahead_char()?;

            if octal_digit_char!(lac) {
                let mut token = String::new();

                while octal_digit_char!(lac) {
                    token.push(lac);

                    self.skip_char()?;
                    lac = self.lookahead_char()?;
                }

                if backslash_char!(lac) {
                    self.skip_char()?;
                    let n = BigInt::parse_bytes(token.as_bytes(), 8).ok_or(ParserError::ParseBigInt)?;

                    match n.to_u8() {
                        Some(i) => Ok(char::from(i)),
                        _ => Err(ParserError::ParseBigInt)
                    }
                } else {
                    put_back_n!(self, token);
                    self.return_char(c);
                    Err(ParserError::UnexpectedChar(lac))
                }
            } else {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c))
            }
        } else {
            Err(ParserError::UnexpectedChar(self.lookahead_char()?))
        }
    }

    fn get_hexadecimal_escape_sequence(&mut self) -> Result<char, ParserError>
    {
        if backslash_char!(self.lookahead_char()?) {
            let c = self.skip_char()?;

            if symbolic_hexadecimal_char!(self.lookahead_char()?) {
                let hex_char = self.skip_char()?;

                let mut lac = self.lookahead_char()?;
                let mut token = String::new();

                while hexadecimal_digit_char!(lac) {
                    token.push(lac);

                    self.skip_char()?;
                    lac = self.lookahead_char()?;
                }

                if backslash_char!(lac) {
                    self.skip_char()?;
                    let n = BigInt::parse_bytes(token.as_bytes(), 16).ok_or(ParserError::ParseBigInt)?;

                    match n.to_u8() {
                        Some(i) => Ok(char::from(i)),
                        _ => Err(ParserError::ParseBigInt)
                    }
                } else {
                    put_back_n!(self, token);

                    self.return_char(hex_char);
                    self.return_char(c);

                    Err(ParserError::UnexpectedChar(lac))
                }
            } else {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c))
            }
        } else {
            Err(ParserError::UnexpectedChar(self.lookahead_char()?))
        }
    }

    fn get_non_quote_char(&mut self) -> Result<char, ParserError> {
        let c = self.lookahead_char()?;

        if graphic_char!(c) || alpha_numeric_char!(c) || solo_char!(c) || space_char!(c) {
            self.skip_char()
        } else {
            self.get_meta_escape_sequence()
                .or_else(|e| {
                    if let ParserError::UnexpectedChar(_) = e {
                        self.get_control_escape_sequence()
                    } else {
                        Err(e)
                    }
                })
                .or_else(|e| {
                    if let ParserError::UnexpectedChar(_) = e {
                        self.get_octal_escape_sequence()
                    } else {
                        Err(e)
                    }
                })
                .or_else(|e| {
                    if let ParserError::UnexpectedChar(_) = e {
                        self.get_hexadecimal_escape_sequence()
                    } else {
                        Err(e)
                    }
                })
        }
    }

    fn char_code_list_token(&mut self) -> Result<String, ParserError> {
        let mut token = String::new();

        self.skip_char()?;
        consume_chars_with!(token, self.get_double_quoted_item());

        if double_quote_char!(self.lookahead_char()?) {
            self.skip_char()?;
            Ok(token)
        } else {
            Err(ParserError::MissingQuote)
        }
    }

    fn hexadecimal_constant(&mut self) -> Result<BigInt, ParserError> {
        self.skip_char()?;
        
        if hexadecimal_digit_char!(self.lookahead_char()?) {
            let mut s = String::new();

            while hexadecimal_digit_char!(self.lookahead_char()?) {
                s.push(self.skip_char()?);
            }

            Ok(BigInt::parse_bytes(s.as_bytes(), 16).ok_or(ParserError::ParseBigInt)?)
        } else {
            self.return_char('x');
            Err(ParserError::ParseBigInt)
        }
    }

    fn octal_constant(&mut self) -> Result<BigInt, ParserError> {
        self.skip_char()?;

        if octal_digit_char!(self.lookahead_char()?) {
            let mut s = String::new();

            while octal_digit_char!(self.lookahead_char()?) {
                s.push(self.skip_char()?);
            }

            Ok(BigInt::parse_bytes(s.as_bytes(), 8).ok_or(ParserError::ParseBigInt)?)
        } else {
            self.return_char('o');
            Err(ParserError::ParseBigInt)
        }
    }

    fn binary_constant(&mut self) -> Result<BigInt, ParserError> {
        self.skip_char()?;

        if binary_digit_char!(self.lookahead_char()?) {
            let mut s = String::new();

            while binary_digit_char!(self.lookahead_char()?) {
                s.push(self.skip_char()?);
            }

            Ok(BigInt::parse_bytes(s.as_bytes(), 2).ok_or(ParserError::ParseBigInt)?)
        } else {
            self.return_char('b');
            Err(ParserError::ParseBigInt)
        }
    }

    fn variable_token(&mut self) -> Result<Token, ParserError> {
        let mut s = String::new();
        s.push(self.skip_char()?);

        while alpha_numeric_char!(self.lookahead_char()?) {
            s.push(self.skip_char()?);
        }

        Ok(Token::Var(rc_atom!(s)))
    }

    fn name_token(&mut self, c: char) -> Result<Token, ParserError> {
        let mut token = String::new();

        if small_letter_char!(c) {
            token.push(self.skip_char()?);

            while alpha_numeric_char!(self.lookahead_char()?) {
                token.push(self.skip_char()?);
            }
        } else if graphic_token_char!(c) {
            token.push(self.skip_char()?);

            while graphic_token_char!(self.lookahead_char()?) {
                token.push(self.skip_char()?);
            }
        } else if cut_char!(c) {
            token.push(self.skip_char()?);
        } else if semicolon_char!(c) {
            token.push(self.skip_char()?);
        } else if single_quote_char!(c) {
            self.skip_char()?;

            consume_chars_with!(token, self.get_single_quoted_item());

            if single_quote_char!(self.lookahead_char()?) {
                self.skip_char()?;

                if let Some(c) = token.chars().next() {
                    if token.len() == 1 {
                        return Ok(Token::Constant(Constant::Char(c)));
                    }
                }
            } else {
                return Err(ParserError::InvalidSingleQuotedCharacter)
            }
        } else {
            match self.get_back_quoted_string() {
                Ok(_)  => return Err(ParserError::BackQuotedString),
                Err(e) => return Err(e)
            }
        }

        Ok(Token::Constant(atom!(token, self.atom_tbl)))
    }

    fn vacate_with_float(&mut self, mut token: String) -> Token {
        self.return_char(token.pop().unwrap());

        let result = Number::Float(OrderedFloat(parse_lossy::<f64, _>(token.as_bytes())));
        Token::Constant(Constant::Number(result))
    }

    pub
    fn number_token(&mut self) -> Result<Token, ParserError> {
        let mut token = String::new();

        token.push(self.skip_char()?);
        let mut c = self.lookahead_char()?;

        while decimal_digit_char!(c) {
            token.push(c);
            self.skip_char()?;
            c = self.lookahead_char()?;
        }

        if decimal_point_char!(c) {
            self.skip_char()?;

            if self.reader.peek().is_none() {
                self.return_char('.');
                let n = BigInt::parse_bytes(token.as_bytes(), 10).ok_or(ParserError::ParseBigInt)?;
                Ok(Token::Constant(integer!(n)))
            } else if decimal_digit_char!(self.lookahead_char()?) {
                token.push('.');
                token.push(self.skip_char()?);

                let mut c = self.lookahead_char()?;

                while decimal_digit_char!(c) {
                    token.push(c);
                    self.skip_char()?;
                    c = self.lookahead_char()?;
                }

                if exponent_char!(self.lookahead_char()?) {
                    token.push(self.skip_char()?);

                    let c = match self.lookahead_char() {
                        Err(_) => return Ok(self.vacate_with_float(token)),
                        Ok(c) => c
                    };

                    if !sign_char!(c) && !decimal_digit_char!(c) {
                        return Ok(self.vacate_with_float(token));
                    }

                    if sign_char!(c) {
                        token.push(self.skip_char()?);

                        let c = match self.lookahead_char() {
                            Err(_) => {
                                self.return_char(token.pop().unwrap());
                                return Ok(self.vacate_with_float(token));
                            },
                            Ok(c) => c
                        };

                        if !decimal_digit_char!(c) {
                            self.return_char(token.pop().unwrap());
                            return Ok(self.vacate_with_float(token));
                        }
                    }

                    if decimal_digit_char!(self.lookahead_char()?) {
                        token.push(self.skip_char()?);

                        while decimal_digit_char!(self.lookahead_char()?) {
                            token.push(self.skip_char()?);
                        }

                        let n = Number::Float(OrderedFloat(parse_lossy::<f64, _>(token.as_bytes())));
                        Ok(Token::Constant(Constant::Number(n)))
                    } else {
                        return Ok(self.vacate_with_float(token));
                    }
                } else {
                    let n = Number::Float(OrderedFloat(parse_lossy::<f64, _>(token.as_bytes())));
                    Ok(Token::Constant(Constant::Number(n)))
                }
            } else {
                self.return_char('.');
                
                let n = BigInt::parse_bytes(token.as_bytes(), 10).ok_or(ParserError::ParseBigInt)?;
                Ok(Token::Constant(integer!(n)))
            }
        } else {
            if token.starts_with('0') && token.len() == 1 {
                if c == 'x' {
                    Ok(Token::Constant(integer!(self.hexadecimal_constant()
                                                    .or_else(|e| {
                                                        if let ParserError::ParseBigInt = e {
                                                            Ok(BigInt::parse_bytes(token.as_bytes(), 10)
                                                               .ok_or(ParserError::ParseBigInt)?)
                                                        } else {
                                                            Err(e)
                                                        }
                                                    })?)))
                } else if c == 'o' {
                    Ok(Token::Constant(integer!(self.octal_constant()
                                                    .or_else(|e| {
                                                        if let ParserError::ParseBigInt = e {
                                                            Ok(BigInt::parse_bytes(token.as_bytes(), 10)
                                                               .ok_or(ParserError::ParseBigInt)?)
                                                        } else {
                                                            Err(e)
                                                        }
                                                    })?)))
                } else if c == 'b' {
                    Ok(Token::Constant(integer!(self.binary_constant()
                                                    .or_else(|e| {
                                                        if let ParserError::ParseBigInt = e {
                                                            Ok(BigInt::parse_bytes(token.as_bytes(), 10)
                                                               .ok_or(ParserError::ParseBigInt)?)
                                                        } else {
                                                            Err(e)
                                                        }
                                                    })?)))
                } else if single_quote_char!(c) {
                    self.skip_char()?;
                    self.get_single_quoted_char()
                        .map(|c| Token::Constant(Constant::CharCode(c as u8)))
                        .or_else(|_| {
                            self.return_char(c);
                            let n = BigInt::parse_bytes(token.as_bytes(), 10)
                                .ok_or(ParserError::ParseBigInt)?;
                            Ok(Token::Constant(integer!(n)))
                        })
                } else {
                    let n = BigInt::parse_bytes(token.as_bytes(), 10)
                        .ok_or(ParserError::ParseBigInt)?;
                    Ok(Token::Constant(integer!(n)))
                }
            } else {
                let n = BigInt::parse_bytes(token.as_bytes(), 10).ok_or(ParserError::ParseBigInt)?;
                Ok(Token::Constant(integer!(n)))
            }
        }
    }

    pub fn scan_for_layout(&mut self) -> Result<bool, ParserError> {
        let mut layout_inserted = false;
        let mut more_layout = true;

        loop {
            let cr = self.lookahead_char();

            match cr {
                Ok(c) if layout_char!(c) => {
                    try!(self.skip_char());
                    layout_inserted = true;
                },
                Ok(c) if c == '%' => {
                    try!(self.single_line_comment());
                    layout_inserted = true;
                },
                Ok(c) if c == '/' =>
                    if try!(self.bracketed_comment()) {
                        layout_inserted = true;
                    } else {
                        more_layout = false;
                    },
                _ => more_layout = false
            };

            if !more_layout {
                break;
            }
        }

        Ok(layout_inserted)
    }

    pub fn next_token(&mut self) -> Result<Token, ParserError> {
        let layout_inserted = try!(self.scan_for_layout());
        let cr = self.lookahead_char();

        match cr {
            Ok(c) => {
                if capital_letter_char!(c) || variable_indicator_char!(c) {
                    return self.variable_token();
                }

                if c == ',' {
                    try!(self.skip_char());
                    return Ok(Token::Comma);
                }

                if c == ')' {
                    try!(self.skip_char());
                    return Ok(Token::Close);
                }

                if c == '(' {
                    try!(self.skip_char());
                    return Ok(if layout_inserted { Token::Open }
                              else { Token::OpenCT });
                }

                if c == '.' {
                    self.skip_char()?;

                    match self.lookahead_char() {
                        Ok(c) if layout_char!(c) || c == '%' => {
                            if new_line_char!(c) {
                                self.skip_char()?;
                            }

                            return Ok(Token::End);
                        },
                        Err(ParserError::UnexpectedEOF) =>
                            return Ok(Token::End),
                        _ => self.return_char('.')
                    };
                }

                if decimal_digit_char!(c) {
                    return self.number_token();
                }

                if c == ']' {
                    try!(self.skip_char());
                    return Ok(Token::CloseList);
                }

                if c == '[' {
                    try!(self.skip_char());
                    return Ok(Token::OpenList);
                }

                if c == '|' {
                    try!(self.skip_char());
                    return Ok(Token::HeadTailSeparator);
                }

                if c == '{' {
                    try!(self.skip_char());
                    return Ok(Token::OpenCurly);
                }

                if c == '}' {
                    try!(self.skip_char());
                    return Ok(Token::CloseCurly);
                }

                if c == '"' {
                    let s = self.char_code_list_token()?;

                    if let DoubleQuotes::Atom = self.flags.double_quotes {
                        let s = clause_name!(s, self.atom_tbl);
                        return Ok(Token::Constant(Constant::Atom(s, None)));
                    } else { // for now.. == DoubleQuotes::Chars
                        let s = StringList::new(s, false);
                        return Ok(Token::Constant(Constant::String(s)));
                    }
                }

                self.name_token(c)
            },
            Err(e) => Err(e)
        }
    }
}
