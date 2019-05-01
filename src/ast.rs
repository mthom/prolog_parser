use num::bigint::{BigInt, BigUint};
use num::{Float, Integer, One, Signed, ToPrimitive, Zero};
use num::rational::{BigRational, Ratio};
use ordered_float::*;
use string_list::*;
use tabled_rc::*;

use put_back_n::*;

use std::cell::Cell;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::io::{Bytes, Error as IOError, Read};
use std::ops::{Add, Div, Sub, Mul, MulAssign, Neg};
use std::rc::Rc;
use std::str::Utf8Error;
use std::vec::Vec;

pub type Atom = String;

pub type Var = String;

pub type Specifier = u32;

pub const MAX_ARITY: usize = 63;

pub const XFX: u32 = 0x0001;
pub const XFY: u32 = 0x0002;
pub const YFX: u32 = 0x0004;
pub const XF: u32  = 0x0010;
pub const YF: u32  = 0x0020;
pub const FX: u32  = 0x0040;
pub const FY: u32  = 0x0080;
pub const DELIMITER: u32 = 0x0100;
pub const TERM: u32  = 0x1000;
pub const LTERM: u32 = 0x3000;

pub const NEGATIVE_SIGN: u32 = 0x0200;

#[macro_export]
macro_rules! clause_name {
    ($name: expr, $tbl: expr) => (
        ClauseName::User(TabledRc::new($name, $tbl.clone()))
    ) ;
    ($name: expr) => (
        ClauseName::BuiltIn($name)
    )
}

#[macro_export]
macro_rules! atom {
    ($e:expr, $tbl:expr) => (
        Constant::Atom(ClauseName::User(tabled_rc!($e, $tbl)), None)
    );
    ($e:expr) => (
        Constant::Atom(clause_name!($e), None)
    )
}

#[macro_export]
macro_rules! integer {
    ($i:expr) => (
        Constant::Number(Number::Integer(Rc::new(BigInt::from($i))))
    )
}

#[macro_export]
macro_rules! rc_atom {
    ($e:expr) => (
        Rc::new(String::from($e))
    )
}
macro_rules! is_term {
    ($x:expr) => ( ($x & TERM) != 0 )
}

macro_rules! is_lterm {
    ($x:expr) => ( ($x & LTERM) != 0 )
}

macro_rules! is_op {
    ($x:expr) => ( $x & (XF | YF | FX | FY | XFX | XFY | YFX) != 0 )
}

macro_rules! is_negate {
    ($x:expr) => ( ($x & NEGATIVE_SIGN) != 0 )
}

#[macro_export]
macro_rules! is_prefix {
    ($x:expr) => ( $x & (FX | FY) != 0 )
}

#[macro_export]
macro_rules! is_postfix {
    ($x:expr) => ( $x & (XF | YF) != 0 )
}

#[macro_export]
macro_rules! is_infix {
    ($x:expr) => ( ($x & (XFX | XFY | YFX)) != 0 )
}

#[macro_export]
macro_rules! is_xfx {
    ($x:expr) => ( ($x & XFX) != 0 )
}

#[macro_export]
macro_rules! is_xfy {
    ($x:expr) => ( ($x & XFY) != 0 )
}

#[macro_export]
macro_rules! is_yfx {
    ($x:expr) => ( ($x & YFX) != 0 )
}

#[macro_export]
macro_rules! is_yf {
    ($x:expr) => ( ($x & YF) != 0 )
}

#[macro_export]
macro_rules! is_xf {
    ($x:expr) => ( ($x & XF) != 0 )
}

#[macro_export]
macro_rules! is_fx {
    ($x:expr) => ( ($x & FX) != 0 )
}

#[macro_export]
macro_rules! is_fy {
    ($x:expr) => ( ($x & FY) != 0 )
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum RegType {
    Perm(usize),
    Temp(usize)
}

impl Default for RegType {
    fn default() -> Self {
        RegType::Temp(0)
    }
}

impl RegType {
    pub fn reg_num(self) -> usize {
        match self {
            RegType::Perm(reg_num) | RegType::Temp(reg_num) => reg_num
        }
    }

    pub fn is_perm(self) -> bool {
        match self {
            RegType::Perm(_) => true,
            _ => false
        }
    }
}

impl fmt::Display for RegType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &RegType::Perm(val) => write!(f, "Y{}", val),
            &RegType::Temp(val) => write!(f, "X{}", val)
        }
    }
}

#[derive(PartialEq, Eq, Clone, Copy)]
pub enum VarReg {
    ArgAndNorm(RegType, usize),
    Norm(RegType)
}

impl VarReg {
    pub fn norm(self) -> RegType {
        match self {
            VarReg::ArgAndNorm(reg, _) | VarReg::Norm(reg) => reg
        }
    }
}

impl fmt::Display for VarReg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &VarReg::Norm(RegType::Perm(reg)) => write!(f, "Y{}", reg),
            &VarReg::Norm(RegType::Temp(reg)) => write!(f, "X{}", reg),
            &VarReg::ArgAndNorm(RegType::Perm(reg), arg) =>
                write!(f, "Y{} A{}", reg, arg),
            &VarReg::ArgAndNorm(RegType::Temp(reg), arg) =>
                write!(f, "X{} A{}", reg, arg)
        }
    }
}

impl Default for VarReg {
    fn default() -> Self {
        VarReg::Norm(RegType::default())
    }
}

#[macro_export]
macro_rules! temp_v {
    ($x:expr) => (
        RegType::Temp($x)
    )
}

#[macro_export]
macro_rules! perm_v {
    ($x:expr) => (
        RegType::Perm($x)
    )
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum GenContext {
    Head, Mid(usize), Last(usize) // Mid & Last: chunk_num
}

impl GenContext {
    pub fn chunk_num(self) -> usize {
        match self {
            GenContext::Head => 0,
            GenContext::Mid(cn) | GenContext::Last(cn) => cn
        }
    }
}

pub type OpDirKey = (ClauseName, Fixity);

#[derive(Clone)]
pub struct OpDirValue(pub SharedOpDesc, pub ClauseName);

impl OpDirValue {
    pub fn new(spec: Specifier, priority: usize, module_name: ClauseName) -> Self {
        OpDirValue(SharedOpDesc::new(priority, spec), module_name)
    }

    #[inline]
    pub fn shared_op_desc(&self) -> SharedOpDesc {
        self.0.clone()
    }

    #[inline]
    pub fn owning_module(&self) -> ClauseName {
        self.1.clone()
    }
}

// name and fixity -> operator type and precedence.
pub type OpDir = HashMap<OpDirKey, OpDirValue>;

#[derive(Clone, Copy)]
pub struct MachineFlags {
    pub double_quotes: DoubleQuotes
}

impl Default for MachineFlags {
    fn default() -> Self {
        MachineFlags { double_quotes: DoubleQuotes::default() }
    }
}

#[derive(Clone, Copy)]
pub enum DoubleQuotes {
    Atom, Chars, Codes
}

impl DoubleQuotes {
    pub fn is_chars(self) -> bool {
        if let DoubleQuotes::Chars = self {
            true
        } else {
            false
        }
    }

    pub fn is_atom(self) -> bool {
        if let DoubleQuotes::Atom = self {
            true
        } else {
            false
        }
    }

    pub fn is_codes(self) -> bool {
        if let DoubleQuotes::Codes = self {
            true
        } else {
            false
        }
    }
}

impl Default for DoubleQuotes {
    fn default() -> Self {
        DoubleQuotes::Chars
    }
}

pub fn default_op_dir() -> OpDir {
    let module_name = clause_name!("builtins");
    let mut op_dir = OpDir::new();

    op_dir.insert((clause_name!(":-"), Fixity::In),  OpDirValue::new(XFX, 1200, module_name.clone()));
    op_dir.insert((clause_name!(":-"), Fixity::Pre), OpDirValue::new(FX,  1200, module_name.clone()));
    op_dir.insert((clause_name!("?-"), Fixity::Pre), OpDirValue::new(FX,  1200, module_name.clone()));
    op_dir.insert((clause_name!(","), Fixity::In),   OpDirValue::new(XFY, 1000, module_name.clone()));

    op_dir
}

#[derive(Debug, Clone, Copy)]
pub enum ArithmeticError {
    InvalidAtom,
    InvalidOp,
    InvalidTerm,
    NoRoots,
    UninstantiatedVar
}

pub enum ParserError {
    Arithmetic(ArithmeticError),
    BackQuotedString,
    BadPendingByte,
    CannotParseCyclicTerm,
    UnexpectedChar(char),
    UnexpectedEOF,
    IO(IOError),
    ExpectedRel,
    ExpectedTopLevelTerm,
    InadmissibleFact,
    InadmissibleQueryTerm,
    IncompleteReduction,
    InconsistentEntry,
    InvalidHook,
    InvalidModuleDecl,
    InvalidModuleExport,
    InvalidRuleHead,
    InvalidUseModuleDecl,
    InvalidModuleResolution,
    InvalidSingleQuotedCharacter,
    MissingQuote,
    NonPrologChar,
    ParseBigInt,
    ParseFloat,
    Utf8Error
}

impl ParserError {
    pub fn as_str(&self) -> &'static str {
        match self {
            &ParserError::Arithmetic(_) =>
                "arithmetic_error",
            &ParserError::BackQuotedString =>
                "back_quoted_string",
            &ParserError::BadPendingByte =>
                "bad_pending_byte",
            &ParserError::UnexpectedChar(_) =>
                "unexpected_char",
            &ParserError::UnexpectedEOF =>
                "unexpected_end_of_file",
            &ParserError::ExpectedRel =>
                "expected_relation",
            &ParserError::ExpectedTopLevelTerm =>
                "expected_atom_or_cons_or_clause",
            &ParserError::InadmissibleFact =>
                "inadmissible_fact",
            &ParserError::InadmissibleQueryTerm =>
                "inadmissible_query_term",
            &ParserError::IncompleteReduction =>
                "incomplete_reduction",
            &ParserError::InconsistentEntry =>
                "inconsistent_entry",
            &ParserError::InvalidHook =>
                "invalid_hook",
            &ParserError::InvalidModuleDecl =>
                "invalid_module_declaration",
            &ParserError::InvalidModuleExport =>
                "invalid_module_export",
            &ParserError::InvalidModuleResolution =>
                "invalid_module_resolution",
            &ParserError::InvalidRuleHead =>
                "invalid_head_of_rule",
            &ParserError::InvalidUseModuleDecl =>
                "invalid_use_module_declaration",
            &ParserError::InvalidSingleQuotedCharacter =>
                "invalid_single_quoted_character",
            &ParserError::IO(_) =>
                "input_output_error",
            &ParserError::MissingQuote =>
                "missing_quote",
            &ParserError::NonPrologChar =>
                "non_prolog_character",
            &ParserError::ParseBigInt =>
                "cannot_parse_big_int",
            &ParserError::ParseFloat =>
                "cannot_parse_float",
            &ParserError::Utf8Error =>
                "utf8_conversion_error",
            &ParserError::CannotParseCyclicTerm =>
                "cannot_parse_cyclic_term"
        }
    }
}

impl From<ArithmeticError> for ParserError {
    fn from(err: ArithmeticError) -> ParserError {
        ParserError::Arithmetic(err)
    }
}

impl From<IOError> for ParserError {
    fn from(err: IOError) -> ParserError {
        ParserError::IO(err)
    }
}

impl From<Utf8Error> for ParserError {
    fn from(_: Utf8Error) -> ParserError {
        ParserError::Utf8Error
    }
}

#[derive(Clone, Copy, Eq, Hash, PartialEq, PartialOrd, Ord)]
pub enum Fixity {
    In, Post, Pre
}

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct SharedOpDesc(Rc<Cell<(usize, Specifier)>>);

impl SharedOpDesc {
    #[inline]
    pub fn new(priority: usize, spec: Specifier) -> Self {
        SharedOpDesc(Rc::new(Cell::new((priority, spec))))
    }

    #[inline]
    pub fn ptr_eq(lop_desc: &SharedOpDesc, rop_desc: &SharedOpDesc) -> bool {
        Rc::ptr_eq(&lop_desc.0, &rop_desc.0)
    }

    #[inline]
    pub fn arity(&self) -> usize {
        if self.get().1 & (XFX | XFY | YFX) == 0 {
            1
        } else {
            2
        }
    }

    #[inline]
    pub fn get(&self) -> (usize, Specifier) {
        self.0.get()
    }

    #[inline]
    pub fn set(&self, prec: usize, spec: Specifier) {
        self.0.set((prec, spec));
    }

    #[inline]
    pub fn prec(&self) -> usize {
        self.0.get().0
    }

    #[inline]
    pub fn assoc(&self) -> Specifier {
        self.0.get().1
    }
}

impl Hash for SharedOpDesc {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.get().hash(state);
    }
}

#[derive(Clone, Hash)]
pub enum Constant {
    Atom(ClauseName, Option<SharedOpDesc>),
    CharCode(u8),
    Char(char),
    Number(Number),
    String(StringList),
    Usize(usize),
    EmptyList
}

impl fmt::Display for Constant {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Constant::Atom(ref atom, _) =>
                if atom.as_str().chars().any(|c| "`.$'\" ".contains(c)) {
                    write!(f, "'{}'", atom.as_str())
                } else {
                    write!(f, "{}", atom.as_str())
                },
            &Constant::Char(c) =>
                write!(f, "'{}'", c as u8),
            &Constant::CharCode(c) =>
                write!(f, "{}", c),
            &Constant::EmptyList =>
                write!(f, "[]"),
            &Constant::Number(ref n) =>
                write!(f, "{}", n),
            &Constant::String(ref s) =>
                write!(f, "\"{}\"", s.borrow()),
            &Constant::Usize(integer) =>
                write!(f, "u{}", integer)
        }
    }
}

impl PartialEq for Constant {
    fn eq(&self, other: &Constant) -> bool {
        match (self, other) {
            (&Constant::Atom(ref atom, _), &Constant::Char(c))
          | (&Constant::Char(c), &Constant::Atom(ref atom, _)) => {
              let s = atom.as_str();
              c.len_utf8() == s.len() && Some(c) == s.chars().next()
            },
            (&Constant::Atom(ref a1, _), &Constant::Atom(ref a2, _)) =>
                a1.as_str() == a2.as_str(),
            (&Constant::Char(c1), &Constant::Char(c2)) =>
                c1 == c2,
            (&Constant::CharCode(c1), &Constant::CharCode(c2)) =>
                c1 == c2,
            (&Constant::CharCode(c1), &Constant::Number(Number::Integer(ref c2)))
          | (&Constant::Number(Number::Integer(ref c2)), &Constant::CharCode(c1)) =>
              match c2.to_u8() {
                  Some(c2) => c1 == c2,
                  None => false
              },
            (&Constant::Number(ref n1), &Constant::Number(ref n2)) =>
                n1 == n2,
            (&Constant::String(ref s1), &Constant::String(ref s2)) =>
                s1 == s2,
            (&Constant::EmptyList, &Constant::EmptyList) =>
                true,
            (&Constant::Usize(u1), &Constant::Usize(u2)) =>
                u1 == u2,
            _ => false
        }
    }
}

impl Eq for Constant {}

impl Constant {
    pub fn to_atom(self) -> Option<ClauseName> {
        match self {
            Constant::Atom(a, _) => Some(a.defrock_brackets()),
            _ => None
        }
    }

    pub fn to_integer(self) -> Option<Rc<BigInt>> {
        match self {
            Constant::Number(Number::Integer(b)) => Some(b),
            _ => None
        }
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum Number {
    Float(OrderedFloat<f64>),
    Integer(Rc<BigInt>),
    Rational(Rc<Ratio<BigInt>>)
}

impl PartialOrd for Number {
    fn partial_cmp(&self, other: &Number) -> Option<Ordering> {
        match NumberPair::from(self.clone(), other.clone()) {
            NumberPair::Integer(n1, n2) =>
                Some(n1.cmp(&n2)),
            NumberPair::Float(n1, n2) =>
                Some(n1.cmp(&n2)),
            NumberPair::Rational(n1, n2) =>
                Some(n1.cmp(&n2))
        }
    }
}

impl Ord for Number {
    fn cmp(&self, other: &Number) -> Ordering {
        match NumberPair::from(self.clone(), other.clone()) {
            NumberPair::Integer(n1, n2) =>
                n1.cmp(&n2),
            NumberPair::Float(n1, n2) =>
                n1.cmp(&n2),
            NumberPair::Rational(n1, n2) =>
                n1.cmp(&n2)
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            &Number::Float(fl) => write!(f, "{}", fl),
            &Number::Integer(ref bi) => write!(f, "{}", bi),
            &Number::Rational(ref r) => write!(f, "{}", r)
        }
    }
}

impl Default for Number {
    fn default() -> Self {
        Number::Float(OrderedFloat(0f64))
    }
}

#[derive(Clone)]
pub enum ClauseName {
    BuiltIn(&'static str),
    User(TabledRc<Atom>)
}

impl fmt::Display for ClauseName {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

impl Hash for ClauseName {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (*self.as_str()).hash(state)
    }
}

impl PartialEq for ClauseName {
    fn eq(&self, other: &ClauseName) -> bool {
        *self.as_str() == *other.as_str()
    }
}

impl Eq for ClauseName {}

impl Ord for ClauseName {
    fn cmp(&self, other: &ClauseName) -> Ordering {
        (*self.as_str()).cmp(other.as_str())
    }
}

impl PartialOrd for ClauseName {
    fn partial_cmp(&self, other: &ClauseName) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<'a> From<&'a TabledRc<Atom>> for ClauseName {
    fn from(name: &'a TabledRc<Atom>) -> ClauseName {
        ClauseName::User(name.clone())
    }
}

impl ClauseName {
    #[inline]
    pub fn owning_module(&self) -> Self {
        match self {
            &ClauseName::User(ref name) => {
                let module = name.owning_module();
                 ClauseName::User(TabledRc { atom: module.clone(),
                                             table: TabledData::new(module) })
            },
            _ => clause_name!("user")
        }
    }

    #[inline]
    pub fn to_rc(&self) -> Rc<String> {
        match self {
            &ClauseName::BuiltIn(s) => Rc::new(s.to_string()),
            &ClauseName::User(ref rc) => rc.inner()
        }
    }

    #[inline]
    pub fn with_table(self, atom_tbl: TabledData<Atom>) -> Self {
        match self {
            ClauseName::BuiltIn(_) => self,
            ClauseName::User(mut name) => {
                name.table = atom_tbl;
                ClauseName::User(name)
            }
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            &ClauseName::BuiltIn(s) => s,
            &ClauseName::User(ref name) => name.as_ref()
        }
    }

    pub fn defrock_brackets(self) -> Self {
        fn defrock_brackets(s: &str) -> &str {
            if s.starts_with('(') && s.ends_with(')') {
                &s[1 .. s.len() - 1]
            } else {
                s
            }
        }

        match self {
            ClauseName::BuiltIn(s) =>
                ClauseName::BuiltIn(defrock_brackets(s)),
            ClauseName::User(s) =>
                ClauseName::User(tabled_rc!(defrock_brackets(s.as_str()).to_owned(), s.table))
        }
    }
}

impl AsRef<str> for ClauseName {
    #[inline]
    fn as_ref(self: &Self) -> &str {
        self.as_str()
    }
}

#[inline]
pub fn binary_pow<T>(mut n: T, mut power: BigUint) -> T
    where T: Clone + Mul + One,
    for<'a> T: MulAssign<&'a T>
{
    use num::pow;

    if power.is_zero() {
        return T::one();
    }

    let mut oddand = T::one();
    let one = BigUint::one();

    while power > one {
        if power.is_odd() {
            oddand *= &n;
        }

        n = pow(n, 2);
        power >>= 1;
    }

    n * oddand
}

fn rational_pow(r1: BigRational, r2: BigRational) -> Result<BigRational, ArithmeticError>
{
    #[inline]
    fn to_unsigned(n: &BigInt) -> Result<BigUint, ArithmeticError> {
        n.abs().to_biguint().ok_or(ArithmeticError::NoRoots)
    };

    #[inline]
    fn to_big_rational(n: BigUint) -> BigRational {
        BigRational::from_integer(BigInt::from(n))
    };

    let r2 = r2.reduced(); // so that gcd(numer, denom) = 1
    let n = to_unsigned(r2.denom())?;

    if n == BigUint::one() {
        return if r2.is_positive() {
            Ok(binary_pow(r1, to_unsigned(&r2.numer())?))
        } else if r2.is_negative() {
            Ok(binary_pow(r1, to_unsigned(&r2.numer())?).recip())
        } else {
            Ok(BigRational::one())
        };
    }

    if (n.is_even() && r1.is_negative()) || (r2.is_negative() && r1.is_zero()) {
        return Err(ArithmeticError::NoRoots);
    }

    let sgn = r1.signum();
    let r1 = r1 * &sgn; // set r1 to its absolute value.

    let epsilon = BigRational::new_raw(BigInt::one(), BigInt::from(10000));
    let n1      = n.clone() - BigUint::one(); // n -1

    // 1 + r1 / (n-1) is a good initial point.
    let mut x_i = BigRational::one() + r1.clone() / to_big_rational(n1.clone());
    let mut x_i_n1 = binary_pow(x_i.clone(), n1.clone()); // x_i^{n-1}
    let mut delta_x_i = BigRational::one();

    while delta_x_i.abs() > epsilon {
        x_i = x_i.reduced();
        x_i_n1 = x_i_n1.reduced();

        let r_quot = r1.clone() / &x_i_n1; // r1 / x_i^{n-1}
        let r_n    = to_big_rational(n.clone());

        delta_x_i = ( r_quot - &x_i ) / &r_n;

        x_i += &delta_x_i;
        x_i_n1 = binary_pow(x_i.clone(), n1.clone());
    }

    if r2.is_positive() {
        Ok(binary_pow(sgn * x_i, to_unsigned(r2.numer())?))
    } else {
        Ok(binary_pow(sgn * x_i, to_unsigned(r2.numer())?).recip())
    }
}

fn pow_float(f1: f64, f2: f64) -> Result<Number, ArithmeticError> {
    let result = OrderedFloat(f1.powf(f2));

    if result.is_finite() {
        Ok(Number::Float(result))
    } else {
        Err(ArithmeticError::NoRoots)
    }
}

fn rational_to_f64(r: &BigRational) -> Option<f64> {
    match (r.numer().to_f64(), r.denom().to_f64()) {
        (Some(ref f1), Some(ref f2)) if f2.is_normal() => Some(*f1 / *f2),
        _ => None
    }
}

impl Number {
    pub fn pow(self, other: Number) -> Result<Self, ArithmeticError> {
        match NumberPair::from(self, other) {
            NumberPair::Integer(n1, n2) =>
                if let Some(n2) = n2.to_biguint() {
                    Ok(Number::Integer(Rc::new(binary_pow((*n1).clone(), n2))))
                } else if n1.is_zero() {
                    Err(ArithmeticError::NoRoots)
                } else {
                    let r1 = Ratio::new(BigInt::one(), (*n1).clone());
                    let n2 = n2.abs().to_biguint().unwrap();

                    Ok(Number::Rational(Rc::new(binary_pow(r1, n2))))
                },
            NumberPair::Float(n1, n2) =>
                pow_float(n1.into_inner(), n2.into_inner()),
            NumberPair::Rational(r1, r2) => {
                if let (Some(f1), Some(f2)) = (rational_to_f64(&r1), rational_to_f64(&r2)) {
                    if let Ok(result) = pow_float(f1, f2) {
                        return Ok(result);
                    }
                }

                let root = rational_pow((*r1).clone(), (*r2).clone())?;
                Ok(Number::Rational(Rc::new(root)))
            }
        }
    }

    #[inline]
    pub fn is_zero(&self) -> bool {
        match self {
            &Number::Float(fl)       => fl.into_inner().is_zero(),
            &Number::Integer(ref bi) => bi.is_zero(),
            &Number::Rational(ref r) => r.is_zero()
        }
    }

    #[inline]
    pub fn is_positive(&self) -> bool {
        match self {
            &Number::Float(fl)       => fl.into_inner().is_sign_positive(),
            &Number::Integer(ref bi) => bi.is_positive(),
            &Number::Rational(ref r) => r.is_positive()
        }
    }

    #[inline]
    pub fn abs(&self) -> Self {
        match self {
            &Number::Float(ref fl)   => Number::Float(OrderedFloat(fl.into_inner().abs())),
            &Number::Integer(ref n)  => Number::Integer(Rc::new((*n).clone().abs())),
            &Number::Rational(ref r) => Number::Rational(Rc::new((*r).clone().abs()))
        }
    }
}

pub enum NumberPair {
    Float(OrderedFloat<f64>, OrderedFloat<f64>),
    Integer(Rc<BigInt>, Rc<BigInt>),
    Rational(Rc<Ratio<BigInt>>, Rc<Ratio<BigInt>>)
}

impl NumberPair {
    fn flip(self) -> NumberPair {
        match self {
            NumberPair::Float(f1, f2)    => NumberPair::Float(f2, f1),
            NumberPair::Integer(n1, n2)  => NumberPair::Integer(n2, n1),
            NumberPair::Rational(r1, r2) => NumberPair::Rational(r2, r1)
        }
    }

    fn integer_float_pair(n1: Rc<BigInt>, n2: OrderedFloat<f64>) -> NumberPair {
        match n1.to_f64() {
            Some(f1) => NumberPair::Float(OrderedFloat(f1), n2),
            None => if let Some(r) = Ratio::from_float(n2.into_inner()) {
                NumberPair::Rational(Rc::new(Ratio::from_integer((*n1).clone())),
                                     Rc::new(r))
            } else if n2.into_inner().is_sign_positive() {
                NumberPair::Float(OrderedFloat(f64::infinity()),
                                  OrderedFloat(f64::infinity()))
            } else {
                NumberPair::Float(OrderedFloat(f64::neg_infinity()),
                                  OrderedFloat(f64::neg_infinity()))
            }
        }
    }

    fn float_rational_pair(n1: OrderedFloat<f64>, n2: Rc<Ratio<BigInt>>) -> NumberPair {
        match (n2.numer().to_f64(), n2.denom().to_f64()) {
            (Some(num), Some(denom)) =>
                NumberPair::Float(n1, OrderedFloat(num / denom)),
            _ => if let Some(r) = Ratio::from_float(n1.into_inner()) {
                NumberPair::Rational(Rc::new(r), n2)
            } else if n1.into_inner().is_sign_positive() {
                NumberPair::Float(OrderedFloat(f64::infinity()),
                                  OrderedFloat(f64::infinity()))
            } else {
                NumberPair::Float(OrderedFloat(f64::neg_infinity()),
                                  OrderedFloat(f64::neg_infinity()))
            }
        }
    }

    pub fn from(n1: Number, n2: Number) -> NumberPair
    {
        match (n1, n2) {
            (Number::Integer(n1), Number::Integer(n2)) =>
                NumberPair::Integer(n1, n2),
            (Number::Float(n1), Number::Float(n2)) =>
                NumberPair::Float(n1, n2),
            (Number::Rational(n1), Number::Rational(n2)) =>
                NumberPair::Rational(n1, n2),
            (Number::Integer(n1), Number::Float(n2)) =>
                Self::integer_float_pair(n1, n2),
            (Number::Float(n1), Number::Integer(n2)) =>
                Self::integer_float_pair(n2, n1).flip(),
            (Number::Float(n1), Number::Rational(n2)) =>
                Self::float_rational_pair(n1, n2),
            (Number::Rational(n1), Number::Float(n2)) =>
                Self::float_rational_pair(n2, n1).flip(),
            (Number::Rational(n1), Number::Integer(n2)) =>
                NumberPair::Rational(n1, Rc::new(Ratio::from_integer((*n2).clone()))),
            (Number::Integer(n1), Number::Rational(n2)) =>
                NumberPair::Rational(Rc::new(Ratio::from_integer((*n1).clone())), n2)
        }
    }
}

impl Add<Number> for Number {
    type Output = Number;

    fn add(self, rhs: Number) -> Self::Output {
        match NumberPair::from(self, rhs) {
            NumberPair::Float(f1, f2) =>
                Number::Float(OrderedFloat(f1.into_inner() + f2.into_inner())),
            NumberPair::Integer(n1, n2) =>
                Number::Integer(Rc::new(&*n1 + &*n2)),
            NumberPair::Rational(r1, r2) =>
                Number::Rational(Rc::new(&*r1 + &*r2))
        }
    }
}

impl Sub<Number> for Number {
    type Output = Number;

    fn sub(self, rhs: Number) -> Self::Output {
        match NumberPair::from(self, rhs) {
            NumberPair::Float(f1, f2) =>
                Number::Float(OrderedFloat(f1.into_inner() - f2.into_inner())),
            NumberPair::Integer(n1, n2) =>
                Number::Integer(Rc::new(&*n1 - &*n2)),
            NumberPair::Rational(r1, r2) =>
                Number::Rational(Rc::new(&*r1 - &*r2))
        }
    }
}

impl Mul<Number> for Number {
    type Output = Number;

    fn mul(self, rhs: Number) -> Self::Output {
        match NumberPair::from(self, rhs) {
            NumberPair::Float(f1, f2) =>
                Number::Float(OrderedFloat(f1.into_inner() * f2.into_inner())),
            NumberPair::Integer(n1, n2) =>
                Number::Integer(Rc::new(&*n1 * &*n2)),
            NumberPair::Rational(r1, r2) =>
                Number::Rational(Rc::new(&*r1 * &*r2))
        }
    }
}

impl Div<Number> for Number {
    type Output = Number;

    fn div(self, rhs: Number) -> Self::Output {
        match NumberPair::from(self, rhs) {
            NumberPair::Float(f1, f2) =>
                Number::Float(OrderedFloat(f1.into_inner() / f2.into_inner())),
            NumberPair::Integer(n1, n2) =>
                match n1.to_f64() {
                    Some(f1) => if let Some(f2) = n2.to_f64() {
                        Number::Float(OrderedFloat(f1 / f2))
                    } else {
                        let r1 = Ratio::from_integer((*n1).clone());
                        let r2 = Ratio::from_integer((*n2).clone());

                        Number::Rational(Rc::new(r1 / r2))
                    },
                    None => {
                        let r1 = Ratio::from_integer((*n1).clone());
                        let r2 = Ratio::from_integer((*n2).clone());

                        Number::Rational(Rc::new(r1 / r2))
                    },
                },
            NumberPair::Rational(r1, r2) =>
                Number::Rational(Rc::new(&*r1 / &*r2))
        }
    }
}

impl Neg for Number {
    type Output = Number;

    fn neg(self) -> Self::Output {
        match self {
            Number::Integer(n) => Number::Integer(Rc::new(-&*n)),
            Number::Float(f) => Number::Float(OrderedFloat(-1.0 * f.into_inner())),
            Number::Rational(r) => Number::Rational(Rc::new(- &*r))
        }
    }
}

#[derive(PartialEq, Eq, Clone)]
pub enum Term {
    AnonVar,
    Clause(Cell<RegType>, ClauseName, Vec<Box<Term>>, Option<SharedOpDesc>),
    Cons(Cell<RegType>, Box<Term>, Box<Term>),
    Constant(Cell<RegType>, Constant),
    Var(Cell<VarReg>, Rc<Var>)
}

impl Term {
    pub fn shared_op_desc(&self) -> Option<SharedOpDesc> {
        match self {
            &Term::Clause(_, _, _, ref spec) => spec.clone(),
            &Term::Constant(_, Constant::Atom(_, ref spec)) => spec.clone(),
            _ => None
        }
    }

    pub fn to_constant(self) -> Option<Constant> {
        match self {
            Term::Constant(_, c) => Some(c),
            _ => None
        }
    }

    pub fn first_arg(&self) -> Option<&Term> {
        match self {
            &Term::Clause(_, _, ref terms, _) =>
                terms.first().map(|bt| bt.as_ref()),
            _ => None
        }
    }

    pub fn name(&self) -> Option<ClauseName> {
        match self {
            &Term::Constant(_, Constant::Atom(ref atom, _))
          | &Term::Clause(_, ref atom, ..) => Some(atom.clone()),
            _ => None
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            &Term::Clause(_, _, ref child_terms, ..) => child_terms.len(),
            _ => 0
        }
    }
}

#[derive(Clone, Copy)]
pub struct CompositeOp<'a, 'b> {
    pub op_dir: &'a OpDir,
    pub static_op_dir: Option<&'b OpDir>
}

#[macro_export]
macro_rules! composite_op {
    ($include_machine_p:expr, $op_dir:expr, $machine_op_dir:expr) => (
        CompositeOp { op_dir: $op_dir,
                      static_op_dir: if !$include_machine_p {
                          Some($machine_op_dir)
                      } else {
                          None
                      }}
    );
    ($op_dir:expr) => (
        CompositeOp { op_dir: $op_dir, static_op_dir: None }
    )
}

impl<'a, 'b> CompositeOp<'a, 'b>
{
    #[inline]
    pub(crate)
    fn get(&self, name: ClauseName, fixity: Fixity) -> Option<OpDirValue>
    {
        self.op_dir.get(&(name.clone(), fixity))
            .or_else(move || self.static_op_dir.and_then(|op_dir| op_dir.get(&(name, fixity))))
            .cloned()
    }
}

pub type ParsingStream<R> = PutBackN<Bytes<R>>;

#[inline]
pub fn parsing_stream<R: Read>(src: R) -> ParsingStream<R> {
    put_back_n(src.bytes())
}
