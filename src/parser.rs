use crate::error::{Error, ParserError, ParserErrorKind as PEK};
use crate::iter::MultiPeek;
use crate::lexer::{TokenIterator, Token, TokenKind as TK};
use crate::position::Position;
use std::collections::BTreeMap;
use std::rc::Rc;

use ExprKind as EK;

#[derive(Clone, Debug)]
pub struct FunctionBean {
    pub params: Vec<String>,
    pub body: Box<Expr>,
}

pub type Block = Vec<Expr>;

#[derive(Clone, Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub position: Position,
}

#[derive(Clone, Debug)]
pub enum ExprKind {
    // Statement-like (can only appear in a semicolon-delimited list of expressions; require
    // semicolon afterwards):
    Continue(Option<Box<Expr>>),
    Break(Option<Box<Expr>>),
    Return(Option<Box<Expr>>),
    Throw(Box<Expr>),
    Let(String, Option<Box<Expr>>),

    // Primary:
    Identifier(String),
    Function(Rc<FunctionBean>),
    Boolean(bool),
    Number(f64),
    String(String),
    Array(Vec<Expr>),
    Object(BTreeMap<String, Expr>),
    Null,
    Paren(Box<Expr>),
    If(Vec<(Expr, Block)>, Option<Block>),
    Try(Block, (String, Block)),
    While(Box<Expr>, Block),
    Loop(Block),
    For(String, Box<Expr>, Block),
    Block(Block),

    // Primary suffix:
    Call(Box<Expr>, Vec<Expr>),
    Index(Box<Expr>, Box<Expr>),
    Dot(Box<Expr>, String),

    // Unary:
    Negative(Box<Expr>),
    Positive(Box<Expr>),
    BoolNot(Box<Expr>),
    BitNegate(Box<Expr>),

    // Binary:
    Equal(Box<Expr>, Box<Expr>),
    NotEqual(Box<Expr>, Box<Expr>),
    LessThan(Box<Expr>, Box<Expr>),
    LessThanEqual(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    GreaterThanEqual(Box<Expr>, Box<Expr>),
    Add(Box<Expr>, Box<Expr>),
    Subtract(Box<Expr>, Box<Expr>),
    Multiply(Box<Expr>, Box<Expr>),
    Divide(Box<Expr>, Box<Expr>),
    Modulo(Box<Expr>, Box<Expr>),
    LeftShift(Box<Expr>, Box<Expr>),
    RightShift(Box<Expr>, Box<Expr>),
    BoolOr(Box<Expr>, Box<Expr>),
    BoolAnd(Box<Expr>, Box<Expr>),
    BoolXor(Box<Expr>, Box<Expr>),
    BitOr(Box<Expr>, Box<Expr>),
    BitAnd(Box<Expr>, Box<Expr>),
    BitXor(Box<Expr>, Box<Expr>),

    // Binary (assignment):
    Assign(Box<Expr>, Box<Expr>),
    LeftShiftAssign(Box<Expr>, Box<Expr>),
    RightShiftAssign(Box<Expr>, Box<Expr>),
    PlusAssign(Box<Expr>, Box<Expr>),
    MinusAssign(Box<Expr>, Box<Expr>),
    MultiplyAssign(Box<Expr>, Box<Expr>),
    DivideAssign(Box<Expr>, Box<Expr>),
    ModuloAssign(Box<Expr>, Box<Expr>),
    OrAssign(Box<Expr>, Box<Expr>),
    AndAssign(Box<Expr>, Box<Expr>),
    XorAssign(Box<Expr>, Box<Expr>),
}

pub struct Parser<'a> {
    tokens: MultiPeek<TokenIterator<'a>>,
    prev_pos: Position,
    next_pos: Position,
}

impl<'a> Parser<'a> {
    pub fn parse_script(script: &str) -> Result<Block, Error> {
        let tokens = MultiPeek::new(TokenIterator::new(script));
        let prev_pos = Position { line: 1, column: 0 };
        let next_pos = tokens.inner().next_pos();
        parse_script(&mut Parser { tokens, prev_pos, next_pos })
    }

    fn next_token(&mut self) -> Result<Option<TK>, Error> {
        match self.tokens.next() {
            Some(Ok(Token { kind, position })) => {
                self.prev_pos = position;
                self.next_pos = self.tokens.inner().next_pos();
                Ok(Some(kind))
            },
            Some(Err(err)) => Err(err),
            None => Ok(None),
        }
    }

    fn peek_token(&mut self, index: usize) -> Result<Option<&TK>, Error> {
        match self.tokens.peek(index) {
            Some(Ok(Token { ref kind, .. })) => {
                Ok(Some(kind))
            },
            Some(Err(err)) => Err(err.clone()),
            None => Ok(None),
        }
    }

    #[inline]
    fn err(&self, kind: PEK, position: Position) -> Error {
        Error::Parser(ParserError { kind, position })
    }
}

macro_rules! expect_next {
    ($parser:expr, $($match_arm:tt)+) => {
        match $parser.next_token()? {
            $($match_arm)+,
            Some(_) => return Err($parser.err(PEK::UnexpectedToken, $parser.prev_pos)),
            None => return Err($parser.err(PEK::UnexpectedEOF, $parser.next_pos)),
        }
    }
}

fn expect_semicolon<'a>(parser: &mut Parser<'a>) -> Result<(), Error> {
    match parser.peek_token(0)? {
        Some(TK::Semicolon) => Ok(()),
        Some(_) => Err(parser.err(PEK::ExpectedSemicolon, parser.next_pos)),
        None => Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
    }
}

fn parse_block<'a>(parser: &mut Parser<'a>) -> Result<Block, Error> {
    match parser.next_token()? {
        Some(TK::LCurly) => (),
        _ => return Err(parser.err(PEK::ExpectedLCurly, parser.prev_pos)),
    }

    parse_block_no_lcurly(parser)
}

fn parse_block_no_lcurly<'a>(parser: &mut Parser<'a>) -> Result<Block, Error> {
    let mut exprs = Vec::new();

    let mut saw_semicolon = true;
    loop {
        match parser.peek_token(0)? {
            Some(TK::RCurly) => {
                parser.next_token()?;
                break;
            },
            Some(TK::Semicolon) => {
                parser.next_token()?;
                saw_semicolon = true;
                continue;
            },
            None => return Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
            Some(_) => {
                if saw_semicolon {
                    exprs.push(parse_block_expr(parser)?);
                    saw_semicolon = false;
                } else {
                    return Err(parser.err(PEK::ExpectedSemicolon, parser.next_pos));
                }
            },
        }
    }

    if saw_semicolon {
        exprs.push(Expr { kind: EK::Null, position: parser.prev_pos });
    }

    Ok(exprs)
}

fn parse_script<'a>(parser: &mut Parser<'a>) -> Result<Block, Error> {
    let mut exprs = Vec::new();

    let mut saw_semicolon = true;
    loop {
        match parser.peek_token(0)? {
            Some(TK::Semicolon) => {
                parser.next_token()?;
                saw_semicolon = true;
                continue;
            },
            None => break,
            Some(_) => {
                if saw_semicolon {
                    exprs.push(parse_block_expr(parser)?);
                    saw_semicolon = false;
                } else {
                    return Err(parser.err(PEK::ExpectedSemicolon, parser.next_pos));
                }
            },
        }
    }

    if saw_semicolon {
        exprs.push(Expr { kind: EK::Null, position: parser.prev_pos });
    }

    Ok(exprs)
}

fn parse_block_expr<'a>(parser: &mut Parser<'a>) -> Result<Expr, Error> {
    let position = parser.next_pos;

    let kind = match parser.peek_token(0)? {
        Some(TK::Break) => parse_flow(parser, TK::Break)?,
        Some(TK::Continue) => parse_flow(parser, TK::Continue)?,
        Some(TK::Return) => parse_flow(parser, TK::Return)?,
        Some(TK::Throw) => parse_throw(parser)?,
        Some(TK::Let) => parse_let(parser)?,
        Some(_) => return parse_expr(parser),
        None => unreachable!(),
    };

    Ok(Expr { kind, position })
}

fn parse_primary<'a>(parser: &mut Parser<'a>) -> Result<Expr, Error> {
    let position = parser.next_pos;

    let kind = match parser.next_token()? {
        Some(TK::Number(value)) => EK::Number(value),
        Some(TK::String(value)) => EK::String(value),
        Some(TK::Identifier(ident)) => EK::Identifier(ident),
        Some(TK::True) => EK::Boolean(true),
        Some(TK::False) => EK::Boolean(false),
        Some(TK::Null) => EK::Null,
        Some(TK::Pipe) => parse_function(parser, false)?,
        Some(TK::Or) => parse_function(parser, true)?,
        Some(TK::LSquare) => parse_array(parser)?,
        Some(TK::LCurly) => parse_block_or_object(parser)?,
        Some(TK::If) => parse_if(parser)?,
        Some(TK::While) => parse_while(parser)?,
        Some(TK::For) => parse_for(parser)?,
        Some(TK::Try) => parse_try(parser)?,
        Some(TK::Loop) => parse_loop(parser)?,
        Some(TK::LParen) => parse_paren(parser)?,
        None => return Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
        _ => return Err(parser.err(PEK::UnexpectedToken, position)),
    };

    let mut expr = Expr { kind, position };

    loop {
        match parser.peek_token(0)? {
            Some(TK::LParen) => expr = parse_call(parser, expr)?,
            Some(TK::LSquare) => expr = parse_index(parser, expr)?,
            Some(TK::Period) => expr = parse_dot(parser, expr)?,
            _ => return Ok(expr),
        }
    }
}

fn parse_call<'a>(parser: &mut Parser<'a>, expr: Expr) -> Result<Expr, Error> {
    parser.next_token()?;
    let position = parser.prev_pos;

    let mut arguments = Vec::new();

    loop {
        match parser.peek_token(0)? {
            Some(TK::RParen) => {
                parser.next_token()?;
                break;
            },
            Some(_) => {
                arguments.push(parse_expr(parser)?);
                match parser.next_token()? {
                    Some(TK::RParen) => break,
                    Some(TK::Comma) => continue,
                    None => return Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
                    _ => return Err(parser.err(PEK::UnexpectedToken, parser.prev_pos)),
                }
            },
            None => return Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
        }
    }

    Ok(Expr { kind: EK::Call(Box::new(expr), arguments), position })
}

fn parse_index<'a>(parser: &mut Parser<'a>, expr: Expr) -> Result<Expr, Error> {
    parser.next_token()?;
    let position = parser.prev_pos;
    let index = parse_expr(parser)?;
    expect_next!(parser, Some(TK::RSquare) => ());
    Ok(Expr { kind: EK::Index(Box::new(expr), Box::new(index)), position })
}

fn parse_dot<'a>(parser: &mut Parser<'a>, expr: Expr) -> Result<Expr, Error> {
    parser.next_token()?;
    let position = parser.prev_pos;
    let ident = expect_next!(parser, Some(TK::Identifier(ident)) => ident);
    Ok(Expr { kind: EK::Dot(Box::new(expr), ident), position })
}

fn parse_if<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    let mut clauses = Vec::new();
    let mut else_block = None;

    loop {
        clauses.push((parse_expr(parser)?, parse_block(parser)?));

        if let Some(TK::Else) = parser.peek_token(0)? {
            parser.next_token()?;
        } else {
            break;
        }

        if let Some(TK::If) = parser.peek_token(0)? {
            parser.next_token()?;
        } else {
            else_block = Some(parse_block(parser)?);
            break;
        }
    }

    Ok(EK::If(clauses, else_block))
}

fn parse_loop<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    Ok(EK::Loop(parse_block(parser)?))
}

fn parse_while<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    Ok(EK::While(Box::new(parse_expr(parser)?), parse_block(parser)?))
}

fn parse_for<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    let ident = expect_next!(parser, Some(TK::Identifier(ident)) => ident);
    expect_next!(parser, Some(TK::In) => ());
    let iter = parse_expr(parser)?;
    let block = parse_block(parser)?;
    Ok(EK::For(ident, Box::new(iter), block))
}

fn parse_try<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    let try_block = parse_block(parser)?;
    expect_next!(parser, Some(TK::Catch) => ());
    let ident = expect_next!(parser, Some(TK::Identifier(ident)) => ident);
    let catch_block = parse_block(parser)?;
    Ok(EK::Try(try_block, (ident, catch_block)))
}

fn parse_flow<'a>(parser: &mut Parser<'a>, kind: TK) -> Result<EK, Error> {
    parser.next_token()?;

    let expr = match parser.peek_token(0)? {
        Some(TK::Semicolon) => None,
        Some(_) => Some(Box::new(parse_expr(parser)?)),
        None => return Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
    };

    expect_semicolon(parser)?;

    match kind {
        TK::Continue => Ok(EK::Continue(expr)),
        TK::Break => Ok(EK::Break(expr)),
        TK::Return => Ok(EK::Return(expr)),
        _ => unreachable!(),
    }
}

fn parse_throw<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    parser.next_token()?;
    let expr = parse_expr(parser)?;
    expect_semicolon(parser)?;
    Ok(EK::Throw(Box::new(expr)))
}

fn parse_let<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    parser.next_token()?;

    let ident = expect_next!(parser, Some(TK::Identifier(ident)) => ident);

    let initial_value = if let Some(TK::Assign) = parser.peek_token(0)? {
        parser.next_token()?;
        Some(Box::new(parse_expr(parser)?))
    } else {
        None
    };

    expect_semicolon(parser)?;

    Ok(EK::Let(ident, initial_value))
}

fn parse_function<'a>(parser: &mut Parser<'a>, skip_params: bool) -> Result<EK, Error> {
    let mut params = Vec::new();

    if !skip_params {
        loop {
            match parser.next_token()? {
                Some(TK::Pipe) => break,
                Some(TK::Identifier(param)) => {
                    params.push(param);
                    match parser.next_token()? {
                        Some(TK::Pipe) => break,
                        Some(TK::Comma) => continue,
                        None => return Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
                        _ => return Err(parser.err(PEK::UnexpectedToken, parser.prev_pos)),
                    }
                },
                None => return Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
                _ => return Err(parser.err(PEK::UnexpectedToken, parser.prev_pos)),
            }
        }
    }

    // Typically, an empty curly brace pair (`{}`) is interpreted as an empty object literal. But in
    // this particular context, we want to treat `{}` as a blank function body.
    //
    // To have the function return an empty object, either `{{}}` or `({})` will suffice. The latter
    // is how it would be done in JavaScript, where a similar case of ambiguity between block and
    // object literal exists.
    let body = if is_next_curly_pair(parser)? {
        let position = parser.next_pos;
        let kind = EK::Block(parse_block(parser)?);
        Expr { kind, position }
    } else {
        parse_expr(parser)?
    };

    Ok(EK::Function(Rc::new(FunctionBean { params, body: Box::new(body) })))
}

fn is_next_curly_pair<'a>(parser: &mut Parser<'a>) -> Result<bool, Error> {
    match parser.peek_token(0)? {
        Some(TK::LCurly) => (),
        _ => return Ok(false),
    }

    Ok(match parser.peek_token(1)? {
        Some(TK::RCurly) => true,
        _ => false,
    })
}

fn parse_array<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    let mut elements = Vec::new();

    loop {
        match parser.peek_token(0)? {
            Some(TK::RSquare) => {
                parser.next_token()?;
                break;
            },
            Some(_) => {
                elements.push(parse_expr(parser)?);
                match parser.next_token()? {
                    Some(TK::RSquare) => break,
                    Some(TK::Comma) => continue,
                    None => return Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
                    _ => return Err(parser.err(PEK::UnexpectedToken, parser.prev_pos)),
                }
            },
            None => return Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
        }
    }

    Ok(EK::Array(elements))
}

fn parse_block_or_object<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    let mut is_object = false;

    loop {
        match parser.peek_token(0)? {
            Some(TK::Identifier(_)) | Some(TK::String(_)) => (),
            Some(TK::RCurly) => {
                is_object = true;
                break;
            },
            _ => break,
        }

        match parser.peek_token(1)? {
            Some(TK::Colon) => (),
            _ => break,
        }

        is_object = true;
        break;
    }

    if is_object {
        parse_object(parser)
    } else {
        Ok(EK::Block(parse_block_no_lcurly(parser)?))
    }
}

fn parse_object<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    let mut pairs = BTreeMap::new();

    loop {
        let key = match parser.next_token()? {
            Some(TK::RCurly) => break,
            Some(TK::Identifier(key)) | Some(TK::String(key)) => key,
            _ => return Err(parser.err(PEK::UnexpectedToken, parser.prev_pos)),
        };

        expect_next!(parser, Some(TK::Colon) => ());
        let value = parse_expr(parser)?;
        pairs.insert(key, value);

        match parser.next_token()? {
            Some(TK::RCurly) => break,
            Some(TK::Comma) => continue,
            None => return Err(parser.err(PEK::UnexpectedEOF, parser.next_pos)),
            _ => return Err(parser.err(PEK::UnexpectedToken, parser.prev_pos)),
        }
    }

    Ok(EK::Object(pairs))
}

fn parse_paren<'a>(parser: &mut Parser<'a>) -> Result<EK, Error> {
    let expr = parse_expr(parser)?;
    match parser.next_token()? {
        Some(TK::RParen) => (),
        _ => return Err(parser.err(PEK::ExpectedRParen, parser.prev_pos)),
    }
    Ok(EK::Paren(Box::new(expr)))
}

fn parse_unary<'a>(parser: &mut Parser<'a>) -> Result<Expr, Error> {
    let position = parser.next_pos;

    let kind = match parser.peek_token(0)? {
        Some(TK::Minus) => {
            parser.next_token()?;
            EK::Negative(Box::new(parse_unary(parser)?))
        },
        Some(TK::Plus) => {
            parser.next_token()?;
            EK::Positive(Box::new(parse_unary(parser)?))
        },
        Some(TK::Bang) => {
            parser.next_token()?;
            EK::BoolNot(Box::new(parse_unary(parser)?))
        },
        Some(TK::Tilde) => {
            parser.next_token()?;
            EK::BitNegate(Box::new(parse_unary(parser)?))
        },
        _ => return parse_primary(parser),
    };

    Ok(Expr { kind, position })
}

#[derive(PartialEq, Eq)]
enum Associativity {
    LTR,
    RTL,
}

fn get_precedence(token: &TK) -> (i8, Associativity) {
    use Associativity::*;

    match *token {
        TK::Divide | TK::Multiply | TK::Modulo => (70, LTR),

        TK::Plus | TK::Minus => (60, LTR),

        TK::LeftShift | TK::RightShift => (50, LTR),

        TK::LessThan | TK::LessThanEqual | TK::GreaterThan | TK::GreaterThanEqual => (41, LTR),
        TK::Equal | TK::NotEqual => (40, LTR),

        TK::Ampersand => (32, LTR),
        TK::Caret => (31, LTR),
        TK::Pipe => (30, LTR),

        TK::And => (22, LTR),
        TK::Xor => (21, LTR),
        TK::Or => (20, LTR),

        TK::Assign | TK::PlusAssign | TK::MinusAssign | TK::MultiplyAssign | TK::DivideAssign |
        TK::LeftShiftAssign | TK::RightShiftAssign | TK::AndAssign | TK::OrAssign | TK::XorAssign |
        TK::ModuloAssign => (10, RTL),

        _ => (-1, LTR),
    }
}

// Precedence climbing method taken from Richards and Whitby-Strevens.
//
// This function is initially called with `prev_prec` as `0`.
fn parse_binary<'a>(parser: &mut Parser<'a>, prev_prec: i8, mut lhs: Expr) -> Result<Expr, Error> {
    loop {
        let (prec, _) = match parser.peek_token(0)? {
            Some(curr_op) => get_precedence(curr_op),
            None => break,
        };

        if prec < prev_prec {
            break;
        }

        // We can `unwrap()` here because a next token is guaranteed to exist; if `peek_token()`
        // returned `None`, we would not be here.
        let operator = parser.next_token()?.unwrap();
        let position = parser.prev_pos;
        let mut rhs = parse_unary(parser)?;

        loop {
            let (next_prec, next_assoc) = match parser.peek_token(0)? {
                Some(next_op) => get_precedence(next_op),
                None => break,
            };

            if prec < next_prec || (prec == next_prec && next_assoc == Associativity::RTL) {
                rhs = parse_binary(parser, next_prec, rhs)?;
            } else {
                break;
            }
        }

        let lhs_kind = match operator {
            TK::Multiply => EK::Multiply(Box::new(lhs), Box::new(rhs)),
            TK::Divide => EK::Divide(Box::new(lhs), Box::new(rhs)),
            TK::Modulo => EK::Modulo(Box::new(lhs), Box::new(rhs)),

            TK::Plus => EK::Add(Box::new(lhs), Box::new(rhs)),
            TK::Minus => EK::Subtract(Box::new(lhs), Box::new(rhs)),

            TK::LeftShift => EK::LeftShift(Box::new(lhs), Box::new(rhs)),
            TK::RightShift => EK::RightShift(Box::new(lhs), Box::new(rhs)),

            TK::LessThan => EK::LessThan(Box::new(lhs), Box::new(rhs)),
            TK::LessThanEqual => EK::LessThanEqual(Box::new(lhs), Box::new(rhs)),
            TK::GreaterThan => EK::GreaterThan(Box::new(lhs), Box::new(rhs)),
            TK::GreaterThanEqual => EK::GreaterThanEqual(Box::new(lhs), Box::new(rhs)),

            TK::Equal => EK::Equal(Box::new(lhs), Box::new(rhs)),
            TK::NotEqual => EK::NotEqual(Box::new(lhs), Box::new(rhs)),

            TK::Ampersand => EK::BitAnd(Box::new(lhs), Box::new(rhs)),
            TK::Caret => EK::BitXor(Box::new(lhs), Box::new(rhs)),
            TK::Pipe => EK::BitOr(Box::new(lhs), Box::new(rhs)),

            TK::And => EK::BoolAnd(Box::new(lhs), Box::new(rhs)),
            TK::Xor => EK::BoolXor(Box::new(lhs), Box::new(rhs)),
            TK::Or => EK::BoolOr(Box::new(lhs), Box::new(rhs)),

            TK::Assign => EK::Assign(Box::new(lhs), Box::new(rhs)),
            TK::PlusAssign => EK::PlusAssign(Box::new(lhs), Box::new(rhs)),
            TK::MinusAssign => EK::MinusAssign(Box::new(lhs), Box::new(rhs)),
            TK::MultiplyAssign => EK::MultiplyAssign(Box::new(lhs), Box::new(rhs)),
            TK::DivideAssign => EK::DivideAssign(Box::new(lhs), Box::new(rhs)),
            TK::LeftShiftAssign => EK::LeftShiftAssign(Box::new(lhs), Box::new(rhs)),
            TK::RightShiftAssign => EK::RightShiftAssign(Box::new(lhs), Box::new(rhs)),
            TK::AndAssign => EK::AndAssign(Box::new(lhs), Box::new(rhs)),
            TK::OrAssign => EK::OrAssign(Box::new(lhs), Box::new(rhs)),
            TK::XorAssign => EK::XorAssign(Box::new(lhs), Box::new(rhs)),
            TK::ModuloAssign => EK::ModuloAssign(Box::new(lhs), Box::new(rhs)),

            _ => return Err(parser.err(PEK::UnexpectedToken, position)),
        };

        lhs = Expr { kind: lhs_kind, position };
    }

    Ok(lhs)
}

fn parse_expr<'a>(parser: &mut Parser<'a>) -> Result<Expr, Error> {
    let lhs = parse_unary(parser)?;
    parse_binary(parser, 0, lhs)
}
