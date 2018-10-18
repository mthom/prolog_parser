use ast::*;
use lexer::*;
use tabled_rc::*;

use std::cell::Cell;
use std::io::Read;
use std::mem::swap;

#[derive(Clone, Copy, PartialEq)]
enum TokenType {
    Term,
    Open,
    OpenCT,
    OpenList,       // '['
    OpenCurly,      // '{'
    HeadTailSeparator, // '|'
    Comma,          // ','
    Close,
    CloseList,      // ']'
    CloseCurly,     // '}'
    End
}

impl TokenType {
    fn is_sep(self) -> bool {
        match self {
            TokenType::HeadTailSeparator | TokenType::OpenCT | TokenType::Open |
            TokenType::Close | TokenType::OpenList | TokenType::CloseList |
            TokenType::OpenCurly | TokenType::CloseCurly | TokenType::Comma
                => true,
            _   => false
        }
    }
}

#[derive(Clone, Copy)]
struct TokenDesc {
    tt: TokenType,
    priority: usize,
    spec: u32
}

fn affirm_xfx(priority: usize, d2: TokenDesc, d3: TokenDesc, d1: TokenDesc) -> bool
{
    d2.priority <= priority
        && is_term!(d3.spec)
        && is_term!(d1.spec)
        && d3.priority < d2.priority
        && d1.priority < d2.priority
}

fn affirm_yfx(priority: usize, d2: TokenDesc, d3: TokenDesc, d1: TokenDesc) -> bool
{
    d2.priority <= priority
        &&    ((is_term!(d3.spec) && d3.priority < d2.priority)
           ||  (is_lterm!(d3.spec) && d3.priority == d2.priority))
        && is_term!(d1.spec)
        && d1.priority < d2.priority
}


fn affirm_xfy(priority: usize, d2: TokenDesc, d3: TokenDesc, d1: TokenDesc) -> bool
{
    d2.priority < priority
        && is_term!(d3.spec)
        && d3.priority < d2.priority
        && is_term!(d1.spec)
        && d1.priority <= d2.priority
}

fn affirm_yf(d1: TokenDesc, d2: TokenDesc) -> bool
{
    is_term!(d2.spec)
        && d2.priority < d1.priority
        && is_lterm!(d2.spec)
        && d2.priority < d1.priority
}

fn affirm_xf(d1: TokenDesc, d2: TokenDesc) -> bool
{
    is_term!(d2.spec) && d2.priority < d1.priority
}

fn affirm_fy(priority: usize, d1: TokenDesc, d2: TokenDesc) -> bool
{
    d2.priority < priority && is_term!(d1.spec) && d1.priority <= d2.priority
}

fn affirm_fx(priority: usize, d1: TokenDesc, d2: TokenDesc) -> bool
{
    d2.priority <= priority && is_term!(d1.spec) && d1.priority < d2.priority
}

fn sep_to_atom(tt: TokenType) -> Option<ClauseName>
{
    match tt {
        TokenType::Open | TokenType::OpenCT =>
            Some(clause_name!("(")),
        TokenType::Close =>
            Some(clause_name!(")")),
        TokenType::OpenList =>
            Some(clause_name!("[")),
        TokenType::CloseList =>
            Some(clause_name!("]")),
        TokenType::OpenCurly =>
            Some(clause_name!("{")),
        TokenType::CloseCurly =>
            Some(clause_name!("}")),
        TokenType::HeadTailSeparator =>
            Some(clause_name!("|")),
        TokenType::Comma =>
            Some(clause_name!(",")),
        TokenType::End =>
            Some(clause_name!(".")),
        _ => None
    }
}

#[derive(Clone, Copy)]
struct OpDesc {
    pre: usize,
    inf: usize,
    post: usize,
    spec: Specifier
}

pub struct Parser<R> where R: Read {
    lexer: Lexer<R>,
    stack: Vec<TokenDesc>,
    terms: Vec<Term>,
}

impl<R: Read> Parser<R> {
    pub fn new(inner: R, atom_tbl: TabledData<Atom>, flags: MachineFlags) -> Self
    {
        Parser { lexer:  Lexer::new(atom_tbl, flags, inner),
                 stack:  Vec::new(),
                 terms:  Vec::new() }
    }

    #[inline]
    pub fn set_atom_tbl(&mut self, atom_tbl: TabledData<Atom>) {
        self.lexer.atom_tbl = atom_tbl;
    }

    pub fn add_to_top(&mut self, head: &str) {
        self.lexer.reader.extend(head.as_bytes().iter().map(|&b| Ok(b)));
    }

    fn get_term_name(&mut self, tt: TokenType) -> Option<ClauseName> {
        match tt {
            TokenType::Comma => Some(clause_name!(",")),
            TokenType::Term =>
                match self.terms.pop() {
                    Some(Term::Constant(_, Constant::Atom(atom, _))) =>
                        Some(atom),
                    Some(term) => {
                        self.terms.push(term);
                        None
                    },
                    _ => None
                },
            _ => None
        }
    }

    fn push_binary_op(&mut self, td: TokenDesc, spec: Specifier)
    {
        if let Some(arg2) = self.terms.pop() {
            if let Some(name) = self.get_term_name(td.tt) {
                if let Some(arg1) = self.terms.pop() {
                    let term = Term::Clause(Cell::default(),
                                            name,
                                            vec![Box::new(arg1), Box::new(arg2)],
                                            Some(Fixity::In));

                    self.terms.push(term);
                    self.stack.push(TokenDesc { tt: TokenType::Term,
                                                priority: td.priority,
                                                spec });
                }
            }
        }
    }

    fn push_unary_op(&mut self, td: TokenDesc, spec: Specifier, fixity: Fixity)
    {
        if let Some(mut arg1) = self.terms.pop() {
            if let Some(mut name) = self.terms.pop() {
                if let Fixity::Post = fixity {
                    swap(&mut arg1, &mut name);
                } else if let Fixity::Pre = fixity {
                    // reduce negation of numbers from structures to numbers.
                    match &name {
                        &Term::Clause(_, ref name, ..)
                      | &Term::Constant(_, Constant::Atom(ref name, _)) if name.as_str() == "-" =>
                          if let Term::Constant(r, Constant::Number(n)) = arg1 {
                              self.terms.push(Term::Constant(r, Constant::Number(-n)));
                              self.stack.push(TokenDesc { tt: TokenType::Term,
                                                          priority: 0,
                                                          spec });

                              return;
                          },
                        _ => {}
                    };
                }

                if let Term::Constant(_, Constant::Atom(name, _)) = name {
                    let term = Term::Clause(Cell::default(), name, vec![Box::new(arg1)],
                                            Some(fixity));

                    self.terms.push(term);
                    self.stack.push(TokenDesc { tt: TokenType::Term,
                                                priority: td.priority,
                                                spec });
                }
            }
        }
    }

    fn get_desc(&self, name: ClauseName, op_dir: CompositeOp) -> Option<OpDesc> {
        let mut op_desc = OpDesc { pre: 0, inf: 0, post: 0, spec: 0 };

        if let Some((spec, pri, _)) = op_dir.get(name.clone(), Fixity::Pre) {
            op_desc.pre = pri;
            op_desc.spec |= spec;
        }

        if let Some((spec, pri, _)) = op_dir.get(name.clone(), Fixity::Post) {
            op_desc.post = pri;
            op_desc.spec |= spec;
        }

        if let Some((spec, pri, _)) = op_dir.get(name.clone(), Fixity::In) {
            op_desc.inf = pri;
            op_desc.spec |= spec;
        }

        if op_desc.pre == 0 && op_desc.post == 0 && op_desc.inf == 0 {
            None
        } else {
            Some(op_desc)
        }
    }

    fn promote_atom_op(&mut self, cell: Cell<RegType>, atom: ClauseName, spec: u32) -> TokenType
    {
        let fixity = if is_infix!(spec) {
            Some(Fixity::In)
        } else if is_prefix!(spec) {
            Some(Fixity::Pre)
        } else if is_postfix!(spec) {
            Some(Fixity::Post)
        } else {
            None
        };

        self.terms.push(Term::Constant(cell, Constant::Atom(atom, fixity)));
        TokenType::Term
    }

    fn shift(&mut self, token: Token, priority: usize, spec: u32) {
        let tt = match token {
            Token::Constant(Constant::Atom(atom, _)) =>
                self.promote_atom_op(Cell::default(), atom, spec),
            Token::Constant(c) => {
                self.terms.push(Term::Constant(Cell::default(), c));
                TokenType::Term
            },
            Token::Var(v) => {
                if v.as_str() == "_" {
                    self.terms.push(Term::AnonVar);
                } else {
                    self.terms.push(Term::Var(Cell::default(), v));
                }

                TokenType::Term
            },
            Token::Comma => TokenType::Comma,
            Token::Open => TokenType::Open,
            Token::Close => TokenType::Close,
            Token::OpenCT => TokenType::OpenCT,
            Token::HeadTailSeparator => TokenType::HeadTailSeparator,
            Token::OpenList => TokenType::OpenList,
            Token::CloseList => TokenType::CloseList,
            Token::OpenCurly => TokenType::OpenCurly,
            Token::CloseCurly => TokenType::CloseCurly,
            Token::End => TokenType::End
        };

        self.stack.push(TokenDesc { tt, priority, spec });
    }

    fn reduce_op(&mut self, priority: usize) {
        loop {
            if let Some(desc1) = self.stack.pop() {
                if let Some(desc2) = self.stack.pop() {
                    if let Some(desc3) = self.stack.pop() {
                        if is_xfx!(desc2.spec) && affirm_xfx(priority, desc2, desc3, desc1)
                        {
                            self.push_binary_op(desc2, LTERM);
                            continue;
                        }
                        else if is_yfx!(desc2.spec) && affirm_yfx(priority, desc2, desc3, desc1)
                        {
                            self.push_binary_op(desc2, LTERM);
                            continue;
                        }
                        else if is_xfy!(desc2.spec) && affirm_xfy(priority, desc2, desc3, desc1)
                        {
                            self.push_binary_op(desc2, TERM);
                            continue;
                        } else {
                            self.stack.push(desc3);
                        }
                    }

                    if is_yf!(desc1.spec) && affirm_yf(desc1, desc2) {
                        self.push_unary_op(desc1, LTERM, Fixity::Post);
                        continue;
                    } else if is_xf!(desc1.spec) && affirm_xf(desc1, desc2) {
                        self.push_unary_op(desc1, LTERM, Fixity::Post);
                        continue;
                    } else if is_fy!(desc2.spec) && affirm_fy(priority, desc1, desc2) {
                        self.push_unary_op(desc2, TERM, Fixity::Pre);
                        continue;
                    } else if is_fx!(desc2.spec) && affirm_fx(priority, desc1, desc2) {
                        self.push_unary_op(desc2, TERM, Fixity::Pre);
                        continue;
                    } else {
                        self.stack.push(desc2);
                        self.stack.push(desc1);
                    }
                } else {
                    self.stack.push(desc1);
                }
            }

            break;
        }
    }

    fn compute_arity_in_brackets(&self) -> Option<usize>
    {
        let mut arity = 0;

        for (i, desc) in self.stack.iter().rev().enumerate() {
            if i % 2 == 0 { // expect a term or non-comma operator.
                if let TokenType::Comma = desc.tt {
                    return None;
                } else if is_term!(desc.spec) || is_op!(desc.spec) {
                    arity += 1;
                } else {
                    return None;
                }
            } else {
                if desc.tt == TokenType::OpenCT {
                    return Some(arity);
                }

                if let TokenType::Comma = desc.tt {
                    continue;
                } else {
                    return None;
                }
            }
        }

        None
    }

    fn reduce_term(&mut self) -> bool
    {
        if self.stack.is_empty() {
            return false;
        }

        self.reduce_op(999);

        let arity = match self.compute_arity_in_brackets() {
            Some(arity) => arity,
            None => return false
        };

        if self.stack.len() > 2 * arity {
            let idx = self.stack.len() - 2 * arity - 1;

            if is_infix!(self.stack[idx].spec) && idx > 0 {
                if !is_op!(self.stack[idx - 1].spec) && !self.stack[idx - 1].tt.is_sep() {
                    return false;
                }
            }
        } else {
            return false;
        }

        let stack_len = self.stack.len() - 2 * arity - 1;
        let idx = self.terms.len() - arity;

        if TokenType::Term == self.stack[stack_len].tt {
            if self.atomize_term(&self.terms[idx - 1]).is_some() {
                self.stack.truncate(stack_len + 1);

                let mut subterms: Vec<_> = self.terms.drain(idx ..)
                    .map(|t| Box::new(t))
                    .collect();

                if let Some(name) = self.terms.pop().and_then(|t| self.atomize_term(&t)) {
                    // reduce the '.' functor to a cons cell if it applies.
                    if name.as_str() == "." && subterms.len() == 2 {
                        let tail = subterms.pop().unwrap();
                        let head = subterms.pop().unwrap();

                        self.terms.push(Term::Cons(Cell::default(), head, tail));
                    } else {
                        self.terms.push(Term::Clause(Cell::default(), name, subterms, None));
                    }

                    if let Some(&mut TokenDesc { ref mut priority, ref mut spec,
                                                 ref mut tt }) = self.stack.last_mut()
                    {
                        *tt = TokenType::Term;
                        *priority = 0;
                        *spec = TERM;
                    }

                    return true;
                }
            }
        }

        false
    }

    pub fn reset(&mut self) {
        self.stack.clear();
    }

    fn compute_arity_in_list(&self) -> Option<usize>
    {
        let mut arity = 0;

        for (i, desc) in self.stack.iter().rev().enumerate() {
            if i % 2 == 0 { // expect a term or non-comma operator.
                if let TokenType::Comma = desc.tt {
                    return None;
                } else if is_term!(desc.spec) || is_op!(desc.spec) {
                    arity += 1;
                } else {
                    return None;
                }
            } else {
                if desc.tt == TokenType::HeadTailSeparator {
                    if arity == 1 {
                        continue;
                    }

                    return None;
                } else if desc.tt == TokenType::OpenList {
                    return Some(arity);
                } else if desc.tt != TokenType::Comma {
                    return None;
                }
            }
        }

        None
    }

    fn reduce_list(&mut self) -> Result<bool, ParserError> {
        if self.stack.is_empty() {
            return Ok(false);
        }

        if let Some(ref mut td) = self.stack.last_mut() {
            if td.tt == TokenType::OpenList {
                td.spec = TERM;
                td.tt = TokenType::Term;
                td.priority = 0;

                self.terms.push(Term::Constant(Cell::default(), Constant::EmptyList));
                return Ok(true);
            }
        }

        self.reduce_op(1000);

        let mut arity = match self.compute_arity_in_list() {
            Some(arity) => arity,
            None => return Ok(false)
        };

        // we know that self.stack.len() >= 2 by this point.
        let idx = self.stack.len() - 2;
        let list_len = self.stack.len() - 2 * arity;

        let end_term = if self.stack[idx].tt != TokenType::HeadTailSeparator {
            Term::Constant(Cell::default(), Constant::EmptyList)
        } else {
            arity -= 1;

            match self.terms.pop() {
                Some(term) => term,
                _ => return Err(ParserError::IncompleteReduction)
            }
        };

        let idx = self.terms.len() - arity;

        let list = self.terms.drain(idx ..)
            .rev()
            .fold(end_term, |acc, t| Term::Cons(Cell::default(),
                                                Box::new(t),
                                                Box::new(acc)));

        self.stack.truncate(list_len);

        self.stack.push(TokenDesc { tt: TokenType::Term, priority: 0, spec: TERM });
        self.terms.push(list);

        Ok(true)
    }

    fn reduce_curly(&mut self) -> Result<bool, ParserError> {
        if self.stack.is_empty() {
            return Ok(false);
        }

        if let Some(ref mut td) = self.stack.last_mut() {
            if td.tt == TokenType::OpenCurly {
                td.tt = TokenType::Term;
                td.priority = 0;
                td.spec = TERM;

                let term = Term::Constant(Cell::default(),
                                          atom!("{}", self.lexer.atom_tbl));
                self.terms.push(term);
                return Ok(true);
            }
        }

        self.reduce_op(1300);

        if self.stack.len() > 1 {
            if let Some(td) = self.stack.pop() {
                if let Some(ref mut oc) = self.stack.last_mut() {
                    if oc.tt == TokenType::OpenCurly {
                        oc.tt = TokenType::Term;
                        oc.priority = 0;
                        oc.spec = TERM;

                        if let Some(atom) = sep_to_atom(td.tt) {
                            let term = Term::Constant(Cell::default(), Constant::Atom(atom, None));
                            self.terms.push(Term::Clause(Cell::default(), clause_name!("{}"),
                                                         vec![Box::new(term)], None));
                        } else {
                            let term = match self.terms.pop() {
                                Some(term) => term,
                                _ => return Err(ParserError::IncompleteReduction)
                            };

                            self.terms.push(Term::Clause(Cell::default(), clause_name!("{}"),
                                                         vec![Box::new(term)], None));
                        }

                        return Ok(true);
                    }
                }
            }
        }

        Ok(false)
    }

    fn reduce_brackets(&mut self) -> bool {
        if self.stack.is_empty() {
            return false;
        }

        self.reduce_op(1300);

        if self.stack.len() == 1 {
            return false;
        }

        let idx = self.stack.len() - 2;

        match self.stack.remove(idx).tt {
            TokenType::Open | TokenType::OpenCT => {
                if let Some(atom) = sep_to_atom(self.stack[idx].tt) {
                    self.terms.push(Term::Constant(Cell::default(), Constant::Atom(atom, None)));
                }

                self.stack[idx].spec = TERM;
                self.stack[idx].tt = TokenType::Term;
                self.stack[idx].priority = 0;
                true
            },
            _ => false
        }
    }

    fn shift_op(&mut self, name: ClauseName, op_dir: CompositeOp) -> Result<bool, ParserError> {
        if let Some(OpDesc { pre, inf, post, spec }) = self.get_desc(name.clone(), op_dir) {
            if pre > 0 && inf + post > 0 {
                match try!(self.lexer.lookahead_char()) {
                    '(' => {
                        // can't be prefix, so either inf == 0
                        // or post == 0.
                        self.reduce_op(inf + post);
                        self.shift(Token::Constant(Constant::Atom(name, None)),
                                   inf + post,
                                   spec & (XFX | XFY | YFX | YF | XF));
                    },
                    _ =>
                        if let Some(&TokenDesc { spec: pspec, .. }) = self.stack.last() {
                            self.reduce_op(inf + post);

                            // rterm.c: 412
                            if is_term!(pspec) {
                                self.shift(Token::Constant(Constant::Atom(name, None)),
                                           inf + post,
                                           spec & (XFX | XFY | YFX | XF | YF));
                            } else {
                                self.shift(Token::Constant(Constant::Atom(name, None)),
                                           pre,
                                           prefix!(spec));
                            }
                        } else {
                            self.shift(Token::Constant(Constant::Atom(name, None)),
                                       pre,
                                       spec & (FX | FY));
                        }
                }
            } else {
                self.reduce_op(pre + inf + post); // only one non-zero priority among these.
                self.shift(Token::Constant(Constant::Atom(name, None)), pre + inf + post, spec);
            }

            Ok(true)
        } else { // not an operator.
            Ok(false)
        }
    }

    fn atomize_term(&self, term: &Term) -> Option<ClauseName> {
        match term {
            &Term::Constant(_, ref c) => self.atomize_constant(c),
            _ => None
        }
    }

    fn atomize_constant(&self, c: &Constant) -> Option<ClauseName> {
        match c {
            &Constant::Atom(ref name, _) => Some(name.clone()),
            &Constant::Char(c) =>
                Some(clause_name!(c.to_string(), self.lexer.atom_tbl)),
            _ => None
        }
    }

    fn shift_token(&mut self, token: Token, op_dir: CompositeOp) -> Result<(), ParserError> {
        match token {
            Token::Constant(c) =>
                if let Some(name) = self.atomize_constant(&c) {
                    if !self.shift_op(name, op_dir)? {
                        self.shift(Token::Constant(c), 0, TERM);
                    }
                } else {
                    self.shift(Token::Constant(c), 0, TERM);
                },
            Token::Var(v) => self.shift(Token::Var(v), 0, TERM),
            Token::Open   => self.shift(Token::Open, 1300, DELIMITER),
            Token::OpenCT => self.shift(Token::OpenCT, 1300, DELIMITER),
            Token::Close  =>
                if !self.reduce_term() {
                    if !self.reduce_brackets() {
                        return Err(ParserError::IncompleteReduction);
                    }
                },
            Token::OpenList  => self.shift(Token::OpenList, 1300, DELIMITER),
            Token::CloseList =>
                if !try!(self.reduce_list()) {
                    return Err(ParserError::IncompleteReduction);
                },
            Token::OpenCurly => self.shift(Token::OpenCurly, 1300, DELIMITER),
            Token::CloseCurly =>
                if !try!(self.reduce_curly()) {
                    return Err(ParserError::IncompleteReduction);
                },
            Token::HeadTailSeparator => {
                self.reduce_op(1000); // see rterm.c:495, SWI Prolog docs.
                self.shift(Token::HeadTailSeparator, 1000, DELIMITER);
            },
            Token::Comma => {
                self.reduce_op(1000);
                self.shift(Token::Comma, 1000, XFY);
            },
            Token::End =>
                match self.stack.last().map(|t| t.tt) {
                    Some(TokenType::Open)
                  | Some(TokenType::OpenCT)
                  | Some(TokenType::OpenList)
                  | Some(TokenType::OpenCurly)
                  | Some(TokenType::HeadTailSeparator)
                  | Some(TokenType::Comma)
                        => return Err(ParserError::IncompleteReduction),
                    _ => {}
                }
        }

        Ok(())
    }

    pub fn eof(&mut self) -> Result<bool, ParserError> {
        self.lexer.eof()
    }

    pub fn read_term(&mut self, op_dir: CompositeOp) -> Result<Term, ParserError> {
        loop {
            let token = try!(self.lexer.next_token());
            let at_end = Token::End == token;

            try!(self.shift_token(token, op_dir));

            if at_end {
                break;
            }
        }

        self.reduce_op(1400);

        if self.terms.len() > 1 {
            return Err(ParserError::IncompleteReduction);
        }

        match self.terms.pop() {
            Some(term) => if self.terms.is_empty() {
                Ok(term)
            } else {
                Err(ParserError::IncompleteReduction)
            },
            _ => Err(ParserError::IncompleteReduction)
        }
    }

    pub fn read(&mut self, op_dir: CompositeOp) -> Result<Vec<Term>, ParserError>
    {
        let mut terms = Vec::new();

        loop {
            terms.push(try!(self.read_term(op_dir)));

            if self.lexer.eof()? {
                break;
            }
        }

        Ok(terms)
    }
}
