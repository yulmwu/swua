pub mod tokens;

use crate::{Position, Span};
use std::fmt;
use tokens::{Token, TokenKind};

#[derive(Debug, Clone, PartialEq)]
pub struct LexingError {
    pub kind: LexingErrorKind,
    pub span: Span,
}

macro_rules! impl_error_kind {
    (
        $(
            $ident:ident$(($($arg:ident: $ty:ty),*))?:
            $fn:ident$(<$gen:ident$(: $gen_1:tt$(+$gen_n:tt)*)?>)? $(($($param_ty:ty),*))? => $fmt:expr
        ),*
    ) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum LexingErrorKind {
            $($ident$(($($ty),*))?,)*
        }

        impl fmt::Display for LexingErrorKind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                match self {
                    $(Self::$ident$(($($arg),*))? => write!(f, $fmt),)*
                }
            }
        }

        impl LexingError {
            pub fn new(kind: LexingErrorKind, span: Span) -> Self {
                Self { kind, span }
            }

            $(
                pub fn $fn<$($gen$(: $gen_1$(+$gen_n)*)?)?>($($($arg: $param_ty,)*)? span: Span) -> Self
                {
                    Self::new(LexingErrorKind::$ident$(($($arg.to_string()),*))?, span)
                }
            )*
        }
    };
}

impl_error_kind! {
    InvalidEscapeSequence(escape_sequence: String): invalid_escape_sequence<T: ToString>(T) => "invalid escape sequence `{escape_sequence}`",
    UnterminatedStringLiteral: unterminated_string_literal => "unterminated string literal",
    UnexpectedCharacter(character: String): unexpected_character<T: ToString>(T) => "unexpected character `{character}`"
}

type Result<T> = std::result::Result<T, LexingError>;

#[derive(Debug, Clone, Default)]
pub struct Lexer {
    pub input: String,
    pub tokens: Vec<Token>,
    position: usize,
    read_position: usize,
    current_char: char,
    current_position: Position,
    indent_stack: Vec<usize>,
    nested_indent: usize,
}

impl Lexer {
    pub fn new(input: String) -> Lexer {
        let input = input
            .split('\n')
            .map(|s| s.trim_end())
            .collect::<Vec<_>>()
            .join("\n");
        let mut t = Lexer {
            input,
            tokens: Vec::new(),
            current_position: Position::new(1, 0),
            indent_stack: vec![0],
            ..Default::default()
        };

        t.read_char();
        t
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.current_char = '\0';
        } else {
            self.current_char = self.input.chars().nth(self.read_position).unwrap();
        }
        self.position = self.read_position;
        self.read_position += 1;
        self.current_position.column += 1;
    }

    fn peek_char(&self) -> char {
        if self.read_position >= self.input.len() {
            '\0'
        } else {
            self.input[self.read_position..]
                .chars()
                .next()
                .unwrap_or('\0')
        }
    }

    fn span_from(&self, start: Position) -> Span {
        Span::new(start, self.current_position)
    }

    pub fn tokenize(&mut self) -> Result<()> {
        self.read_indent()?;
        loop {
            self.next_token()?;
            if self.tokens.last().unwrap().kind == TokenKind::EOF {
                break;
            }
        }

        Ok(())
    }

    fn read_indent(&mut self) -> Result<()> {
        let start_position = self.current_position;

        let mut indent: usize = 0;
        loop {
            match self.current_char {
                ' ' => {
                    indent += 1;
                    self.read_char();
                }
                '\t' => {
                    indent += 4;
                    self.read_char();
                }
                '\r' => {
                    self.read_char();
                }
                _ => break,
            }
        }

        if indent == 0 && self.current_char == '\n' && self.peek_char() != '\0' {
            return Ok(());
        }

        use std::cmp::Ordering::*;
        match indent.cmp(self.indent_stack.last().unwrap()) {
            Greater => {
                while indent > *self.indent_stack.last().unwrap() {
                    self.indent_stack
                        .push(*self.indent_stack.last().unwrap() + 4);
                    self.tokens.push(Token::new(
                        TokenKind::Indent,
                        self.span_from(start_position),
                    ));
                }
            }
            Less => {
                while indent < *self.indent_stack.last().unwrap() {
                    self.indent_stack.pop();
                    self.tokens.push(Token::new(
                        TokenKind::Dedent,
                        self.span_from(start_position),
                    ));
                }
            }
            _ => {}
        }

        Ok(())
    }

    fn read_identifier(&mut self) -> Result<()> {
        let position = self.position;
        let start_position = self.current_position;

        while self.current_char.is_alphanumeric() || self.current_char == '_' {
            self.read_char();
        }

        let value = &self.input[position..self.position];

        self.tokens.push(Token::new(
            TokenKind::from(value),
            self.span_from(start_position),
        ));
        Ok(())
    }

    fn read_number(&mut self) -> Result<()> {
        let position = self.position;
        let start_position = self.current_position;
        let mut is_float = false;

        while self.current_char.is_numeric() || self.current_char == '.' {
            if self.current_char == '.' {
                if is_float {
                    break;
                }

                is_float = true;
            }

            self.read_char();
        }

        let token = if is_float {
            TokenKind::Float(self.input[position..self.position].parse::<f64>().unwrap())
        } else {
            TokenKind::Int(self.input[position..self.position].parse::<i64>().unwrap())
        };

        self.tokens
            .push(Token::new(token, self.span_from(start_position)));
        Ok(())
    }

    fn read_string(&mut self) -> Result<()> {
        let start_position = self.current_position;

        self.read_char();
        let mut value = String::new();
        loop {
            match self.current_char {
                '"' => {
                    self.read_char();
                    break;
                }
                '\\' => {
                    let start_position = self.current_position;
                    self.read_char();
                    match self.current_char {
                        'n' => {
                            value.push('\n');
                            self.read_char();
                        }
                        'r' => {
                            value.push('\r');
                            self.read_char();
                        }
                        't' => {
                            value.push('\t');
                            self.read_char();
                        }
                        '\\' => {
                            value.push('\\');
                            self.read_char();
                        }
                        '"' => {
                            value.push('"');
                            self.read_char();
                        }
                        _ => {
                            return Err(LexingError::invalid_escape_sequence(
                                self.input[self.position..self.read_position].to_string(),
                                self.span_from(start_position),
                            ))
                        }
                    }
                }
                '\n' | '\0' => {
                    return Err(LexingError::unterminated_string_literal(
                        self.span_from(start_position),
                    ))
                }
                c => {
                    value.push(c);
                    self.read_char();
                }
            }
        }

        self.tokens.push(Token::new(
            TokenKind::String(value.clone()),
            self.span_from(start_position),
        ));
        Ok(())
    }

    fn single(&mut self, kind: TokenKind) -> Result<()> {
        let token = Token::new(kind, self.span_from(self.current_position));
        self.read_char();
        self.tokens.push(token);
        Ok(())
    }

    fn double(&mut self, kind_1: TokenKind, char_2: char, kind_2: TokenKind) -> Result<()> {
        let start_position = self.current_position;

        self.read_char();
        let token = if self.current_char == char_2 {
            self.read_char();
            Token::new(kind_2, self.span_from(start_position))
        } else {
            Token::new(kind_1, self.span_from(start_position))
        };
        self.tokens.push(token);
        Ok(())
    }

    fn triple(
        &mut self,
        kind_1: TokenKind,
        char_2: char,
        kind_2: TokenKind,
        char_3: char,
        kind_3: TokenKind,
    ) -> Result<()> {
        let start_position = self.current_position;

        self.read_char();
        let token = if self.current_char == char_2 {
            self.read_char();
            Token::new(kind_2, self.span_from(start_position))
        } else if self.current_char == char_3 {
            self.read_char();
            Token::new(kind_3, self.span_from(start_position))
        } else {
            Token::new(kind_1, self.span_from(start_position))
        };
        self.tokens.push(token);
        Ok(())
    }

    fn next_token(&mut self) -> Result<()> {
        let start_position = self.current_position;

        match self.current_char {
            ' ' | '\t' | '\r' => {
                self.read_char();
                self.next_token()
            }
            '\n' => {
                self.current_position.line += 1;
                self.current_position.column = 0;

                if self.nested_indent == 0 {
                    self.tokens.push(Token::new(
                        TokenKind::Newline,
                        self.span_from(start_position),
                    ));
                    self.read_char();
                    self.read_indent()
                } else {
                    self.read_char();
                    self.next_token()
                }
            }
            c if c.is_alphabetic() || c == '_' => self.read_identifier(),
            c if c.is_numeric() => self.read_number(),
            '"' => self.read_string(),
            '=' => self.triple(
                TokenKind::Assign,
                '=',
                TokenKind::EQ,
                '>',
                TokenKind::DoubleArrow,
            ),
            '+' => self.single(TokenKind::Plus),
            '-' => self.double(TokenKind::Minus, '>', TokenKind::Arrow),
            '!' => self.double(TokenKind::Bang, '=', TokenKind::NEQ),
            '*' => self.single(TokenKind::Asterisk),
            '/' => {
                if self.peek_char() == '/' {
                    self.read_char();
                    while self.current_char != '\n' && self.current_char != '\0' {
                        self.read_char();
                    }
                    self.next_token()
                } else {
                    self.single(TokenKind::Slash)
                }
            }
            '%' => self.single(TokenKind::Percent),
            '&' => self.single(TokenKind::Ampersand),
            '@' => self.single(TokenKind::At),
            '#' => self.single(TokenKind::Sharp),
            '.' => self.single(TokenKind::Dot),
            ',' => self.single(TokenKind::Comma),
            ':' => self.double(TokenKind::Colon, ':', TokenKind::DoubleColon),
            ';' => self.single(TokenKind::Semicolon),
            '(' => {
                self.nested_indent += 1;
                self.single(TokenKind::LParen)
            }
            ')' => {
                self.nested_indent -= 1;
                self.single(TokenKind::RParen)
            }
            '{' => {
                self.nested_indent += 1;
                self.single(TokenKind::LBrace)
            }
            '}' => {
                self.nested_indent -= 1;
                self.single(TokenKind::RBrace)
            }
            '[' => {
                self.nested_indent += 1;
                self.single(TokenKind::LBracket)
            }
            ']' => {
                self.nested_indent -= 1;
                self.single(TokenKind::RBracket)
            }
            '<' => self.double(TokenKind::LT, '=', TokenKind::LTE),
            '>' => self.double(TokenKind::GT, '=', TokenKind::GTE),
            '|' => self.single(TokenKind::Pipe),
            '\0' => {
                self.tokens
                    .push(Token::new(TokenKind::EOF, self.span_from(start_position)));
                Ok(())
            }
            c => Err(LexingError::unexpected_character(
                c.to_string(),
                self.span_from(start_position),
            )),
        }
    }
}
