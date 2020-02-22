use lexical::parse_lossy;
use ordered_float::*;
use rug::Integer;

use ast::*;
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
                Err(ParserError::UnexpectedChar(..)) => break,
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
    pub(crate) line_num: usize,
    pub(crate) col_num: usize
}

impl<'a, R: Read> Lexer<'a, R> {
    pub fn new(atom_tbl: TabledData<Atom>, flags: MachineFlags, src: &'a mut ParsingStream<R>) -> Self
    {
        Lexer { atom_tbl, flags, reader: src, line_num: 0, col_num: 0 }
    }

    fn return_char(&mut self, c: char) {
        if new_line_char!(c) {
            self.line_num -= 1;
            self.col_num = 0;
        }

        self.reader.put_back(Ok(c));
    }

    fn skip_char(&mut self) -> Result<char, ParserError> {
        if let Some(Ok(c)) = self.reader.next() {
            self.col_num += 1;

            if new_line_char!(c) {
                self.line_num += 1;
                self.col_num = 0;
            }

            Ok(c)
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
            Some(&Ok(c)) => Ok(c),
            _ => Err(ParserError::UnexpectedEOF),
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
                    Err(ParserError::NonPrologChar(self.line_num, self.col_num))
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
                Err(ParserError::UnexpectedChar(c, self.line_num, self.col_num))
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
                Err(ParserError::UnexpectedChar(c, self.line_num, self.col_num))
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
                Err(ParserError::MissingQuote(self.line_num, self.col_num))
            }
        } else {
            Err(ParserError::UnexpectedChar(c, self.line_num, self.col_num))
        }
    }

    fn get_single_quoted_item(&mut self) -> Result<Option<char>, ParserError>
    {
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
                Err(ParserError::UnexpectedChar(c, self.line_num, self.col_num))
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
                Err(ParserError::UnexpectedChar(c, self.line_num, self.col_num))
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
                Err(ParserError::UnexpectedChar(c, self.line_num, self.col_num))
            }
        } else {
            Err(ParserError::UnexpectedChar(self.lookahead_char()?, self.line_num, self.col_num))
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
                    '0' => Ok('\u{0}'),
                    c   => {
                        self.return_char(c);
                        Err(ParserError::UnexpectedChar(c, self.line_num, self.col_num))
                    }
                }
            } else {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c, self.line_num, self.col_num))
            }
        } else {
            Err(ParserError::UnexpectedChar(self.lookahead_char()?, self.line_num, self.col_num))
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
                    let n = Integer::from_str_radix(&token, 8)
                          .map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?;
                    
                    match n.to_u8() {
                        Some(i) => Ok(char::from(i)),
                        _ => Err(ParserError::ParseBigInt(self.line_num, self.col_num))
                    }
                } else {
                    put_back_n!(self, token);
                    self.return_char(c);
                    Err(ParserError::UnexpectedChar(lac, self.line_num, self.col_num))
                }
            } else {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c, self.line_num, self.col_num))
            }
        } else {
            Err(ParserError::UnexpectedChar(self.lookahead_char()?, self.line_num, self.col_num))
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
                    let n = Integer::from_str_radix(&token, 16)
                            .map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?;

                    match n.to_u8() {
                        Some(n) => Ok(n as char),
                        _ => Err(ParserError::ParseBigInt(self.line_num, self.col_num))
                    }
                } else {
                    put_back_n!(self, token);

                    self.return_char(hex_char);
                    self.return_char(c);

                    Err(ParserError::UnexpectedChar(lac, self.line_num, self.col_num))
                }
            } else {
                self.return_char(c);
                Err(ParserError::UnexpectedChar(c, self.line_num, self.col_num))
            }
        } else {
            Err(ParserError::UnexpectedChar(self.lookahead_char()?, self.line_num, self.col_num))
        }
    }

    fn get_non_quote_char(&mut self) -> Result<char, ParserError> {
        let c = self.lookahead_char()?;

        if graphic_char!(c) || alpha_numeric_char!(c) || solo_char!(c) || space_char!(c) {
            self.skip_char()
        } else {
            self.get_meta_escape_sequence()
                .or_else(|e| {
                    if let ParserError::UnexpectedChar(..) = e {
                        self.get_octal_escape_sequence()
                    } else {
                        Err(e)
                    }
                })
                .or_else(|e| {
                    if let ParserError::UnexpectedChar(..) = e {
                        self.get_hexadecimal_escape_sequence()
                    } else {
                        Err(e)
                    }
                })
                .or_else(|e| {
                    if let ParserError::UnexpectedChar(..)= e {
                        self.get_control_escape_sequence()
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
            Err(ParserError::MissingQuote(self.line_num, self.col_num))
        }
    }

    fn hexadecimal_constant(&mut self) -> Result<Integer, ParserError> {
        self.skip_char()?;
        
        if hexadecimal_digit_char!(self.lookahead_char()?) {
            let mut s = String::new();

            while hexadecimal_digit_char!(self.lookahead_char()?) {
                s.push(self.skip_char()?);
            }

            Ok(Integer::from_str_radix(&s, 16)
               .map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?)
        } else {
            self.return_char('x');
            Err(ParserError::ParseBigInt(self.line_num, self.col_num))
        }
    }

    fn octal_constant(&mut self) -> Result<Integer, ParserError> {
        self.skip_char()?;

        if octal_digit_char!(self.lookahead_char()?) {
            let mut s = String::new();

            while octal_digit_char!(self.lookahead_char()?) {
                s.push(self.skip_char()?);
            }

            Ok(Integer::from_str_radix(&s, 8)
               .map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?)
        } else {
            self.return_char('o');
            Err(ParserError::ParseBigInt(self.line_num, self.col_num))
        }
    }

    fn binary_constant(&mut self) -> Result<Integer, ParserError> {
        self.skip_char()?;

        if binary_digit_char!(self.lookahead_char()?) {
            let mut s = String::new();

            while binary_digit_char!(self.lookahead_char()?) {
                s.push(self.skip_char()?);
            }

            Ok(Integer::from_str_radix(&s, 2)
               .map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?)
        } else {
            self.return_char('b');
            Err(ParserError::ParseBigInt(self.line_num, self.col_num))
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
                return Err(ParserError::InvalidSingleQuotedCharacter(self.lookahead_char()?))
            }
        } else {
            match self.get_back_quoted_string() {
                Ok(_)  => return Err(ParserError::BackQuotedString(self.line_num, self.col_num)),
                Err(e) => return Err(e)
            }
        }

        Ok(Token::Constant(atom!(token, self.atom_tbl)))
    }

    fn vacate_with_float(&mut self, mut token: String) -> Token {
        self.return_char(token.pop().unwrap());

        let result = OrderedFloat(parse_lossy::<f64, _>(token.as_bytes()));
        Token::Constant(Constant::Float(result))
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
                let n = token.parse::<Integer>()
                       .map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?;
                Ok(Token::Constant(Constant::Integer(n)))
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

                        let n = OrderedFloat(parse_lossy::<f64, _>(token.as_bytes()));
                        Ok(Token::Constant(Constant::Float(n)))
                    } else {
                        return Ok(self.vacate_with_float(token));
                    }
                } else {
                    let n = OrderedFloat(parse_lossy::<f64, _>(token.as_bytes()));
                    Ok(Token::Constant(Constant::Float(n)))
                }
            } else {
                self.return_char('.');
                let n = token.parse::<Integer>()
                        .map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?;
                Ok(Token::Constant(Constant::Integer(n)))
            }
        } else {
            if token.starts_with('0') && token.len() == 1 {
                if c == 'x' {
                    Ok(Token::Constant(Constant::Integer(self.hexadecimal_constant()
                                                             .or_else(|e| {
                                                                 if let ParserError::ParseBigInt(..) = e {
                                                                     Ok(token.parse::<Integer>()
                                                                        .map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?)
                                                                 } else {
                                                                     Err(e)
                                                                 }
                                                             })?)))
                } else if c == 'o' {
                    Ok(Token::Constant(Constant::Integer(self.octal_constant()
                                                             .or_else(|e| {
                                                                 if let ParserError::ParseBigInt(..) = e {
                                                                     Ok(token.parse::<Integer>()
                                                                        .map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?)
                                                                 } else {
                                                                     Err(e)
                                                                 }
                                                             })?)))
                } else if c == 'b' {
                    Ok(Token::Constant(Constant::Integer(self.binary_constant()
                                                             .or_else(|e| {
                                                                 if let ParserError::ParseBigInt(..) = e {
                                                                     Ok(token.parse::<Integer>()
                                                                        .map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?)
                                                                 } else {
                                                                     Err(e)
                                                                 }
                                                             })?)))
                } else if single_quote_char!(c) {
                    self.skip_char()?;
                    self.get_single_quoted_char()
                        .map(|c| Token::Constant(Constant::CharCode(c as u32)))
                        .or_else(|_| {
                            self.return_char(c);
                            let n = token.parse::<Integer>().map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?;
                            Ok(Token::Constant(Constant::Integer(n)))
                        })
                } else {
                    let n = token.parse::<Integer>().map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?;
                    Ok(Token::Constant(Constant::Integer(n)))
                }
            } else {
                let n = token.parse::<Integer>().map_err(|_| ParserError::ParseBigInt(self.line_num, self.col_num))?;
                Ok(Token::Constant(Constant::Integer(n)))
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
                    self.skip_char()?;
                    layout_inserted = true;
                },
                Ok(c) if c == '%' => {
                    self.single_line_comment()?;
                    layout_inserted = true;
                },
                Ok(c) if c == '/' =>
                    if self.bracketed_comment()? {
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
        let layout_inserted = self.scan_for_layout()?;
        let cr = self.lookahead_char();

        match cr {
            Ok(c) => {
                if capital_letter_char!(c) || variable_indicator_char!(c) {
                    return self.variable_token();
                }

                if c == ',' {
                    self.skip_char()?;
                    return Ok(Token::Comma);
                }

                if c == ')' {
                    self.skip_char()?;
                    return Ok(Token::Close);
                }

                if c == '(' {
                    self.skip_char()?;
                    return Ok(if layout_inserted { Token::Open }
                              else { Token::OpenCT });
                }

                if c == '.' {
                    self.skip_char()?;

                    match self.lookahead_char() {
                        Ok(c) if layout_char!(c) || c == '%' => {
                            if new_line_char!(c) {
                                self.skip_char()?;
                            } else {
                                if let Ok(c) = self.lookahead_char() {
                                    if new_line_char!(c) {
                                        self.skip_char()?;
                                    }
                                }
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
                    self.skip_char()?;
                    return Ok(Token::CloseList);
                }

                if c == '[' {
                    self.skip_char()?;
                    return Ok(Token::OpenList);
                }

                if c == '|' {
                    self.skip_char()?;
                    return Ok(Token::HeadTailSeparator);
                }

                if c == '{' {
                    self.skip_char()?;
                    return Ok(Token::OpenCurly);
                }

                if c == '}' {
                    self.skip_char()?;
                    return Ok(Token::CloseCurly);
                }

                if c == '"' {
                    let s = self.char_code_list_token()?;

                    if let DoubleQuotes::Atom = self.flags.double_quotes {
                        let s = clause_name!(s, self.atom_tbl);
                        return Ok(Token::Constant(Constant::Atom(s, None)));
                    } else {
                        let s = Rc::new(s);
                        return Ok(Token::Constant(Constant::String(0, s)));
                    }
                }

                self.name_token(c)
            },
            Err(e) => Err(e)
        }
    }
}
