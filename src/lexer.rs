use crate::error::{Error, LexerError, LexerErrorKind};
use crate::iter::CharsWithPosition;
use crate::position::Position;
use std::char;
use std::str::Chars;
use TokenKind as TK;

#[derive(Debug, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub position: Position,
}

#[derive(Debug, PartialEq)]
pub enum TokenKind {
    Number(f64),
    String(String),
    Identifier(String),
    True,
    False,
    Null,
    Let,
    If,
    Else,
    While,
    For,
    In,
    Loop,
    Break,
    Continue,
    Return,
    Throw,
    Try,
    Catch,
    LCurly,
    RCurly,
    LParen,
    RParen,
    LSquare,
    RSquare,
    Period,
    Semicolon,
    Colon,
    Comma,
    Tilde,
    Assign,
    Equal,
    Bang,
    NotEqual,
    LessThan,
    LessThanEqual,
    LeftShift,
    LeftShiftAssign,
    GreaterThan,
    GreaterThanEqual,
    RightShift,
    RightShiftAssign,
    Plus,
    PlusAssign,
    Minus,
    MinusAssign,
    Multiply,
    MultiplyAssign,
    Divide,
    DivideAssign,
    Modulo,
    ModuloAssign,
    Pipe,
    Or,
    OrAssign,
    Ampersand,
    And,
    AndAssign,
    Caret,
    Xor,
    XorAssign,
}

pub struct TokenIterator<'a> {
    chars: CharsWithPosition<Chars<'a>>,
    position: Position,
}

impl<'a> TokenIterator<'a> {
    pub fn new(script: &'a str) -> TokenIterator<'a> {
        TokenIterator {
            chars: CharsWithPosition::new(script.chars()),
            position: Position { line: 1, column: 0 },
        }
    }

    pub fn next_pos(&self) -> Position {
        self.chars.next_pos()
    }

    fn inner_next(&mut self) -> Result<Option<TK>, LexerErrorKind> {
        while let Some(c) = self.chars.next() {
            self.position = self.chars.prev_pos();
            let kind = match c {
                '0'...'9' => self.number(c)?,
                'A'...'Z' | 'a'...'z' | '_' => self.ident(c),
                '"' => self.string()?,
                '/' => if let Some(tk) = self.slash() { tk } else { continue },
                '{' => TK::LCurly,
                '}' => TK::RCurly,
                '(' => TK::LParen,
                ')' => TK::RParen,
                '[' => TK::LSquare,
                ']' => TK::RSquare,
                '.' => TK::Period,
                ';' => TK::Semicolon,
                ':' => TK::Colon,
                ',' => TK::Comma,
                '~' => TK::Tilde,
                '=' => self.eq(TK::Assign, TK::Equal),
                '!' => self.eq(TK::Bang, TK::NotEqual),
                '+' => self.eq(TK::Plus, TK::PlusAssign),
                '-' => self.eq(TK::Minus, TK::MinusAssign),
                '*' => self.eq(TK::Multiply, TK::MultiplyAssign),
                '%' => self.eq(TK::Modulo, TK::ModuloAssign),
                '|' => self.dbl_eq('|', TK::Pipe, TK::Or, TK::OrAssign),
                '&' => self.dbl_eq('&', TK::Ampersand, TK::And, TK::AndAssign),
                '^' => self.dbl_eq('^', TK::Caret, TK::Xor, TK::XorAssign),
                '<' => self.angle('<'),
                '>' => self.angle('>'),
                c if c.is_whitespace() => continue,
                _ => return Err(LexerErrorKind::UnexpectedChar),
            };

            // Peek ahead and consume any whitespace until the next token. This is important so that
            // `TokenIterator::next_pos()` indicates the position of the start of the next token and
            // not the next whitespace character.
            loop {
                match self.chars.peek(0) {
                    Some(c) if c.is_whitespace() => self.chars.next(),
                    _ => return Ok(Some(kind)),
                };
            }
        }

        Ok(None)
    }

    fn number(&mut self, first_char: char) -> Result<TK, LexerErrorKind> {
        let mut can_support_radix = first_char == '0';
        let mut can_support_decimal = true;
        let mut can_support_exponent = true;
        let mut radix: Option<u32> = None;
        let mut stop = false;
        let mut string = String::new();
        string.push(first_char);

        while let Some(&c) = self.chars.peek(0) {
            match c {
                '0'...'9' => {
                    string.push(c);
                    self.chars.next();
                    can_support_exponent = true;
                },
                '.' if can_support_decimal => {
                    can_support_decimal = false;
                    match self.chars.peek(1) {
                        Some(&c) if c.is_ascii_digit() => {
                            string.push('.');
                            self.chars.next();
                        },
                        _ => break,
                    }
                },
                'e' if can_support_exponent => {
                    string.push('e');
                    self.chars.next();
                    while let Some(&exp) = self.chars.peek(0) {
                        match exp {
                            '0'...'9' | '-' | '+' => {
                                string.push(exp);
                                self.chars.next();
                            },
                            '_' => {
                                self.chars.next();
                            },
                            c if c.is_ascii_alphanumeric() => {
                                return Err(LexerErrorKind::MalformedNumber);
                            },
                            _ => {
                                stop = true;
                                break;
                            },
                        }
                    }
                },
                '_' => {
                    self.chars.next();
                    continue;
                },
                'x' | 'X' | 'o' | 'O' | 'b' | 'B' if can_support_radix => {
                    self.number_radix(c, &mut radix, &mut string)?;
                    stop = true;
                },
                c if c.is_ascii_alphanumeric() => {
                    return Err(LexerErrorKind::MalformedNumber);
                },
                _ => break,
            }

            can_support_radix = false;
            if stop {
                break;
            }
        }

        if let Some(radix) = radix {
            if let Ok(val) = u64::from_str_radix(&string, radix) {
                return Ok(TK::Number(val as f64));
            }
        } else if let Ok(val) = string.parse::<f64>() {
            return Ok(TK::Number(val));
        }

        Err(LexerErrorKind::MalformedNumber)
    }

    fn number_radix(
        &mut self,
        indicator: char,
        out_radix: &mut Option<u32>,
        string: &mut String,
    ) -> Result<(), LexerErrorKind> {
        let radix = match indicator {
            'x' | 'X' => 16,
            'o' | 'O' => 8,
            'b' | 'B' => 2,
            _ => unreachable!(),
        };

        *out_radix = Some(radix);
        string.clear();
        self.chars.next();
        while let Some(&digit) = self.chars.peek(0) {
            match digit {
                '_' => { self.chars.next(); },
                c if c.is_digit(radix) => {
                    string.push(digit);
                    self.chars.next();
                },
                c if c.is_ascii_alphanumeric() => return Err(LexerErrorKind::MalformedNumber),
                _ => break,
            }
        }
        Ok(())
    }

    #[inline]
    fn string(&mut self) -> Result<TK, LexerErrorKind> {
        let mut result = String::new();
        let mut escape = false;

        while let Some(c) = self.chars.next() {
            match c {
                '\\' if !escape => {
                    escape = true;
                    continue;
                },
                '\\' if escape => result.push('\\'),
                't' if escape => result.push('\t'),
                'n' if escape => result.push('\n'),
                'r' if escape => result.push('\r'),
                'x' | 'u' | 'U' if escape => {
                    let mut out_val: u32 = 0;
                    let length = match c {
                        'x' => 2,
                        'u' => 4,
                        'U' => 8,
                        _ => unreachable!(),
                    };
                    for _ in 0..length {
                        if let Some(c) = self.chars.next() {
                            if let Some(d1) = c.to_digit(16) {
                                out_val *= 16;
                                out_val += d1;
                            } else {
                                return Err(LexerErrorKind::MalformedEscapeSequence);
                            }
                        } else {
                            return Err(LexerErrorKind::MalformedEscapeSequence);
                        }
                    }

                    if let Some(r) = char::from_u32(out_val) {
                        result.push(r);
                    } else {
                        return Err(LexerErrorKind::MalformedEscapeSequence);
                    }
                },
                '\'' if escape => result.push('\''),
                '"' if escape => result.push('"'),
                '"' if !escape => break,
                _ if escape => return Err(LexerErrorKind::MalformedEscapeSequence),
                c => result.push(c),
            }

            escape = false;
        }

        Ok(TK::String(result))
    }

    #[inline]
    fn ident(&mut self, first_char: char) -> TK {
        let mut result = String::new();
        result.push(first_char);

        loop {
            match self.chars.peek(0) {
                Some(&c) if c.is_ascii_alphanumeric() || c == '_' => {
                    self.chars.next();
                    result.push(c);
                },
                _ => break,
            }
        }

        match result.as_str() {
            "null" => TK::Null,
            "true" => TK::True,
            "false" => TK::False,
            "let" => TK::Let,
            "if" => TK::If,
            "else" => TK::Else,
            "while" => TK::While,
            "for" => TK::For,
            "in" => TK::In,
            "loop" => TK::Loop,
            "break" => TK::Break,
            "continue" => TK::Continue,
            "return" => TK::Return,
            "throw" => TK::Throw,
            "try" => TK::Try,
            "catch" => TK::Catch,
            _ => TK::Identifier(result),
        }
    }

    #[inline]
    fn slash(&mut self) -> Option<TK> {
        match self.chars.peek(0) {
            Some('/') => {
                self.chars.next();
                while let Some(c) = self.chars.next() {
                    if c == '\n' {
                        break;
                    }
                }
                None
            },
            Some('*') => {
                let mut depth = 1;
                self.chars.next();
                while let Some(c) = self.chars.next() {
                    match c {
                        '/' => if let Some('*') = self.chars.next() {
                            depth += 1;
                        },
                        '*' => if let Some('/') = self.chars.next() {
                            depth -= 1;
                        },
                        _ => (),
                    }

                    if depth == 0 {
                        break;
                    }
                }
                None
            },
            Some('=') => {
                self.chars.next();
                Some(TK::DivideAssign)
            },
            _ => Some(TK::Divide),
        }
    }

    #[inline]
    fn eq(&mut self, bare: TK, eq: TK) -> TK {
        match self.chars.peek(0) {
            Some('=') => {
                self.chars.next();
                eq
            },
            _ => bare,
        }
    }

    #[inline]
    fn dbl_eq(&mut self, c: char, bare: TK, dbl: TK, eq: TK) -> TK {
        match self.chars.peek(0) {
            Some(x) if *x == c => {
                self.chars.next();
                dbl
            },
            Some('=') => {
                self.chars.next();
                eq
            },
            _ => bare,
        }
    }

    #[inline]
    fn angle(&mut self, c: char) -> TK {
        let (cmp, cmp_eq, dbl, dbl_eq) = match c {
            '<' => (TK::LessThan, TK::LessThanEqual, TK::LeftShift, TK::LeftShiftAssign),
            '>' => (TK::GreaterThan, TK::GreaterThanEqual, TK::RightShift, TK::RightShiftAssign),
            _ => unreachable!(),
        };

        match self.chars.peek(0) {
            Some(x) if *x == c => {
                self.chars.next();
                self.eq(dbl, dbl_eq)
            },
            Some('=') => {
                self.chars.next();
                cmp_eq
            },
            _ => cmp,
        }
    }
}

impl<'a> Iterator for TokenIterator<'a> {
    type Item = Result<Token, Error>;

    fn next(&mut self) -> Option<Self::Item> {
        match self.inner_next() {
            Ok(Some(kind)) => Some(Ok(Token {
                kind,
                position: self.position,
            })),
            Ok(None) => None,
            Err(kind) => Some(Err(Error::Lexer(LexerError {
                kind,
                position: self.position,
            }))),
        }
    }
}

#[cfg(test)]
mod tests {
    macro_rules! assert_lexer_ok {
        ($script:expr, $($variant:ident $(($($variant_arg:expr),+ $(,)?))?),* $(,)?) => {
            let mut tokens = super::TokenIterator::new($script);

            $(
                match tokens.next() {
                    Some(Ok(ref t))
                        if t.kind == super::TokenKind::$variant $(($($variant_arg),+))?  => (),
                    None => panic!("unexpected end of token iterator"),
                    token => panic!("unexpected token: {:?}", token),
                }
            )*

            assert!(tokens.next().is_none());
        }
    }

    macro_rules! assert_lexer_err {
        ($script:expr, $variant:ident $(($($variant_arg:expr),+ $(,)?))?) => {
            let mut tokens = super::TokenIterator::new($script);

            loop {
                match tokens.next() {
                    Some(Ok(_)) => (),
                    Some(Err(super::Error::Lexer(super::LexerError {
                        kind: super::LexerErrorKind::$variant $(($($variant_arg),+))?,
                        ..
                    }))) => break,
                    None => panic!("unexpected end of token iterator"),
                    token => panic!("unexpected token: {:?}", token),
                }
            }
        }
    }

    #[test]
    fn number() {
        assert_lexer_ok!("0", Number(0.0));
        assert_lexer_ok!("123456", Number(123456.0));
        assert_lexer_ok!("18446744073709551615", Number(18446744073709552000.0));
        assert_lexer_ok!("18446744073709551616", Number(18446744073709552000.0));
        assert_lexer_ok!("123.456", Number(123.456));
        assert_lexer_ok!("123.456.", Number(123.456), Period);
        assert_lexer_ok!("123456.", Number(123456.0), Period);
        assert_lexer_ok!("123456e1.a", Number(1234560.0), Period, Identifier("a".into()));
        assert_lexer_ok!("123456e1", Number(1234560.0));
        assert_lexer_ok!("123456e123", Number(123456e123));
        assert_lexer_ok!("123456e-123", Number(123456e-123));
        assert_lexer_ok!("1.7976931348623157e308", Number(1.7976931348623157e308));
        assert_lexer_ok!("1.7976931348623157e309", Number(1.0 / 0.0));
        assert_lexer_ok!("2.2250738585072014e-308", Number(2.2250738585072014e-308));
        assert_lexer_ok!("2.2250738585072014e-350", Number(2.2250738585072014e-350));
        assert_lexer_err!("123456a", MalformedNumber);
        assert_lexer_err!("123456e", MalformedNumber);

        assert_lexer_ok!("0xFFFFFFFFFFFFFFFF", Number(18446744073709552000.0));
        assert_lexer_ok!("0xBEEF", Number(48879.0));
        assert_lexer_ok!("0xbeef", Number(48879.0));
        assert_lexer_ok!("0XBEEF", Number(48879.0));
        assert_lexer_ok!("0XBeEf", Number(48879.0));
        assert_lexer_ok!("0o755", Number(493.0));
        assert_lexer_ok!("0O755", Number(493.0));
        assert_lexer_ok!("0b10101", Number(21.0));
        assert_lexer_ok!("0B10101", Number(21.0));
        assert_lexer_ok!("0_X_B_e__E_f___", Number(48879.0));
        assert_lexer_err!("0xFFFFFFFFFFFFFFFFF", MalformedNumber);
        assert_lexer_err!("0o7558", MalformedNumber);
        assert_lexer_err!("0xbeefg1", MalformedNumber);
        assert_lexer_err!("0b101012", MalformedNumber);
        assert_lexer_err!("0x", MalformedNumber);
        assert_lexer_err!("0b", MalformedNumber);
        assert_lexer_err!("0o", MalformedNumber);
        assert_lexer_err!("0X", MalformedNumber);
        assert_lexer_err!("0B", MalformedNumber);
        assert_lexer_err!("0O", MalformedNumber);
    }

    #[test]
    fn keyword() {
        assert_lexer_ok!("null", Null);
        assert_lexer_ok!("true", True);
        assert_lexer_ok!("false", False);
        assert_lexer_ok!("let", Let);
        assert_lexer_ok!("if", If);
        assert_lexer_ok!("else", Else);
        assert_lexer_ok!("while", While);
        assert_lexer_ok!("for", For);
        assert_lexer_ok!("in", In);
        assert_lexer_ok!("loop", Loop);
        assert_lexer_ok!("break", Break);
        assert_lexer_ok!("continue", Continue);
        assert_lexer_ok!("return", Return);
        assert_lexer_ok!("throw", Throw);
        assert_lexer_ok!("try", Try);
        assert_lexer_ok!("catch", Catch);
        assert_lexer_ok!("true false", True, False);
    }

    #[test]
    fn identifier() {
        assert_lexer_ok!("abc", Identifier("abc".into()));
        assert_lexer_ok!("a__b_c", Identifier("a__b_c".into()));
        assert_lexer_ok!("abc123def", Identifier("abc123def".into()));
    }

    #[test]
    fn string() {
        assert_lexer_ok!("\"abc\"", String("abc".into()));
        assert_lexer_ok!("\"12漢34\"", String("12漢34".into()));
        let escapes = "\"\\\\\\t\\n\\r\\x22\\u12ab\\U0010ffff\\\"\\'\"";
        assert_lexer_ok!(escapes, String("\\\t\n\r\x22\u{12ab}\u{10ffff}\"'".into()));
    }
}
