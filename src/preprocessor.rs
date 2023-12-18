use crate::{
    codegen::Identifier,
    identifier,
    lexer::tokens::{Token, TokenKind},
    parser::{ParseResult, ParsingError},
    Span,
};

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Preprocessor<T>
where
    T: Iterator<Item = Token> + Default,
{
    tokens: T,
    current_token: Token,
    peek_token: Token,
    span: Span,
    pub defines: Vec<(String, Vec<Token>)>,
}

impl<T> Preprocessor<T>
where
    T: Iterator<Item = Token> + Default,
{
    pub fn new(tokens: T) -> Self {
        let mut preprocessor = Self {
            tokens,
            ..Default::default()
        };

        preprocessor.next_token();
        preprocessor.next_token();

        preprocessor
    }

    pub fn preprocess(&mut self) -> ParseResult<Vec<Token>> {
        let mut tokens = Vec::new();

        while self.current_token.kind != TokenKind::EOF {
            match self.current_token.kind.clone() {
                TokenKind::Sharp => self.preprocess_directive()?,
                TokenKind::Identifier(identifier) => {
                    self.preprocess_identifier(&mut tokens, identifier)?
                }
                _ => tokens.push(self.current_token.clone()),
            }

            self.next_token();
        }

        tokens.push(Token {
            kind: TokenKind::EOF,
            span: self.span,
        });

        Ok(tokens)
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.span = self.current_token.span;

        self.peek_token = self.tokens.next().unwrap_or(Token {
            kind: TokenKind::EOF,
            span: self.span,
        });
    }

    fn expect_token_consume(&mut self, expected: TokenKind) -> ParseResult<()> {
        if self.current_token.kind == expected {
            self.next_token();
            Ok(())
        } else {
            Err(ParsingError::expected_next_token(
                expected.to_string(),
                self.current_token.kind.to_string(),
                self.current_token.span,
            ))
        }
    }

    fn is_eof(&self) -> bool {
        self.current_token.kind == TokenKind::EOF
    }

    fn preprocess_directive(&mut self) -> ParseResult<()> {
        self.expect_token_consume(TokenKind::Sharp)?;

        match self.current_token.kind {
            TokenKind::Define => self.preprocess_define()?,
            TokenKind::Defln => self.preprocess_defln()?,
            _ => {
                return Err(ParsingError::unexpected_token(
                    self.current_token.kind.to_string(),
                    self.current_token.span,
                ))
            }
        }

        Ok(())
    }

    fn preprocess_define(&mut self) -> ParseResult<()> {
        self.expect_token_consume(TokenKind::Define)?;

        let identifier = identifier! { self };
        self.next_token();

        let mut tokens = Vec::new();

        while self.current_token.kind != TokenKind::Newline && !self.is_eof() {
            tokens.push(self.current_token.clone());
            self.next_token();
        }

        self.defines.push((identifier.identifier, tokens));

        Ok(())
    }

    fn preprocess_defln(&mut self) -> ParseResult<()> {
        self.expect_token_consume(TokenKind::Defln)?;

        /*
        #defln FOO
        ..
        #end
        */

        let identifier = identifier! { self };
        self.next_token();

        let mut tokens = Vec::new();

        while !(self.is_eof()
            || self.current_token.kind == TokenKind::Sharp
                && self.peek_token.kind == TokenKind::End)
        {
            tokens.push(self.current_token.clone());
            self.next_token();
        }

        self.next_token();

        self.defines.push((identifier.identifier, tokens));

        Ok(())
    }

    fn preprocess_identifier(
        &mut self,
        tokens: &mut Vec<Token>,
        identifier: String,
    ) -> ParseResult<()> {
        if let Some((_, replacement)) = self.defines.iter().find(|(name, _)| name == &identifier) {
            replacement
                .iter()
                .for_each(|token| tokens.push(token.clone()));
        } else {
            tokens.push(self.current_token.clone());
        }

        Ok(())
    }
}
