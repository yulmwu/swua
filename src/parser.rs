use crate::{
    codegen::*,
    tokenizer::{Lexer, Token, TokenKind},
    AstArrayTypeKind, AstType, AstTypeKind, BinaryOperator, Position, Priority, Program,
    UnaryOperator,
};
use std::{collections::BTreeMap, fmt};

#[derive(Debug, Clone, PartialEq)]
pub struct ParsingError {
    pub kind: ParsingErrorKind,
    pub position: Position,
}

impl ParsingError {
    pub fn new(kind: ParsingErrorKind, position: Position) -> Self {
        Self { kind, position }
    }

    pub fn expected_next_token<T>(expected: T, got: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::ExpectedNextToken(expected.to_string(), got.to_string()),
            position,
        )
    }

    pub fn expected_ty<T>(expected: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(ParsingErrorKind::ExpectedTy(expected.to_string()), position)
    }

    pub fn expected_expression<T>(expected: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::ExpectedExpression(expected.to_string()),
            position,
        )
    }

    pub fn unexpected_token<T>(token: T, position: Position) -> Self
    where
        T: ToString,
    {
        Self::new(
            ParsingErrorKind::UnexpectedToken(token.to_string()),
            position,
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
#[rustfmt::skip]
pub enum ParsingErrorKind {
    ExpectedNextToken(String, String),
    ExpectedTy(String),
    ExpectedExpression(String),
    UnexpectedToken(String),
}

impl fmt::Display for ParsingErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ParsingErrorKind::ExpectedNextToken(expected, got) => {
                write!(f, "expected `{expected}` but got `{got}`")
            }
            ParsingErrorKind::ExpectedTy(expected) => write!(f, "expected type `{expected}`"),
            ParsingErrorKind::ExpectedExpression(expected) => {
                write!(f, "expected expression `{expected}`")
            }
            ParsingErrorKind::UnexpectedToken(token) => write!(f, "unexpected token `{token}`"),
        }
    }
}

pub type ParseResult<T> = Result<T, ParsingError>;

macro_rules! ident_token_to_string {
    ($self:ident) => {
        match $self.current_token.kind {
            $crate::tokenizer::TokenKind::IDENT(ref ident) => ident.to_string(),
            _ => {
                return Err(ParsingError::expected_next_token(
                    "Identifier".to_string(),
                    $self.current_token.kind.to_string(),
                    $self.current_token.position,
                ))
            }
        }
    };
}

macro_rules! expect_semicolon_without_next_token {
    ($self:ident) => {
        if $self.current_token.kind != $crate::tokenizer::TokenKind::Semicolon {
            return Err(ParsingError::expected_next_token(
                $crate::tokenizer::TokenKind::Semicolon.to_string(),
                $self.current_token.kind.to_string(),
                $self.position,
            ));
        }
    };
}

#[derive(Debug, Default)]
pub struct Parser<'a> {
    pub lexer: Lexer<'a>,
    pub errors: Vec<ParsingError>,
    current_token: Token<'a>,
    peek_token: Token<'a>,
    position: Position,
}

impl<'a> From<&'a str> for Parser<'a> {
    fn from(x: &'a str) -> Self {
        Parser::new(Lexer::new(x))
    }
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        Parser {
            lexer,
            ..Default::default()
        }
    }

    fn next_token(&mut self) {
        self.current_token = self.peek_token.clone();
        self.peek_token = self.lexer.next_token();

        self.position = Position::new(
            self.current_token.position.line,
            self.current_token.position.column,
        );
    }

    fn expect_token(&mut self, token_type: &TokenKind) -> ParseResult<()> {
        if self.current_token.kind == *token_type {
            self.next_token();

            Ok(())
        } else {
            Err(ParsingError::expected_expression(
                self.current_token.kind.to_string(),
                self.position,
            ))
        }
    }

    fn peek_token(&self, token_type: &TokenKind) -> bool {
        self.peek_token.kind == *token_type
    }

    /*
        Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
    MemberAccess,
     */
    fn get_priority(&self, token_type: &TokenKind) -> Priority {
        use Priority::*;
        use TokenKind::*;
        match token_type {
            Assign | EQ | NEQ => Equals,
            LT | GT | LTE | GTE => LessGreater,
            Plus | Minus => Sum,
            Slash | Asterisk => Product,
            Typeof | Sizeof => Prefix,
            LParen => Call,
            LBracket => Index,
            Dot => MemberAccess,
            _ => Lowest,
        }
    }

    fn peek_priority(&mut self) -> Priority {
        self.get_priority(&self.peek_token.kind)
    }

    fn current_priority(&self) -> Priority {
        self.get_priority(&self.current_token.kind)
    }

    pub fn parse_program(&mut self) -> Result<Program, Vec<ParsingError>> {
        self.next_token();
        self.next_token();

        let mut program = Program::default();

        while self.current_token.kind != TokenKind::EOF {
            match self.parse_statement() {
                Ok(statement) => program.statements.push(statement),
                Err(error) => self.errors.push(error),
            }

            self.next_token();
        }

        if !self.errors.is_empty() {
            Err(self.errors.clone())
        } else {
            Ok(program)
        }
    }

    fn parse_statement(&mut self) -> ParseResult<Statement> {
        Ok(match self.current_token.kind {
            TokenKind::Let => Statement::LetStatement(self.parse_let_statement()?),
            TokenKind::Function => Statement::FunctionDefinition(self.parse_function_definition()?),
            TokenKind::Extern => {
                Statement::ExternalFunctionDeclaration(self.parse_external_function_declaration()?)
            }
            TokenKind::Return => Statement::Return(self.parse_return_statement()?),
            TokenKind::Type => Statement::TypeDeclaration(self.parse_type_statement()?),
            TokenKind::Declare => Statement::Declaration(self.parse_declare_statement()?),
            TokenKind::Struct => Statement::StructDeclaration(self.parse_struct_declaration()?),
            _ => self.parse_expression_statement()?,
        })
    }

    fn parse_let_statement(&mut self) -> ParseResult<LetStatement> {
        self.next_token();

        let ident = Identifier {
            identifier: ident_token_to_string! { self },
            position: self.position,
        };
        self.next_token();

        let ty = if self.current_token.kind == TokenKind::Colon {
            self.next_token();
            Some(self.parse_ty()?)
        } else {
            None
        };

        self.expect_token(&TokenKind::Assign)?;

        let expression = self.parse_expression(&Priority::Lowest)?;
        self.next_token();

        Ok(LetStatement {
            name: ident,
            ty,
            value: expression,
            position: self.position,
        })
    }

    fn parse_function_definition(&mut self) -> ParseResult<FunctionDefinition> {
        self.next_token();

        let ident = ident_token_to_string! { self };

        self.next_token();
        self.expect_token(&TokenKind::LParen)?;

        let mut parameters = Vec::new();

        while self.current_token.kind != TokenKind::RParen {
            if let TokenKind::IDENT(identifier) = self.current_token.kind.clone() {
                let identifier_position = self.position;
                self.next_token();
                self.expect_token(&TokenKind::Colon)?;

                let ty = self.parse_ty()?;

                parameters.push(Parameter {
                    name: Identifier {
                        identifier: identifier.to_string(),
                        position: identifier_position,
                    },
                    ty,
                });
            } else {
                return Err(ParsingError::unexpected_token(
                    self.current_token.kind.to_string(),
                    self.position,
                ));
            }

            if self.current_token.kind == TokenKind::RParen {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        self.expect_token(&TokenKind::RParen)?;
        self.expect_token(&TokenKind::Arrow)?;

        let return_type = self.parse_ty()?;

        let body = match self.current_token.kind {
            TokenKind::LBrace => self.parse_block_expression()?,
            TokenKind::DoubleArrow => {
                self.next_token();

                let block = BlockExpression {
                    statements: vec![Statement::Return(ReturnStatement {
                        value: self.parse_expression(&Priority::Lowest)?,
                        position: self.position,
                    })],
                    position: self.position,
                };

                self.next_token();

                expect_semicolon_without_next_token! { self }

                block
            }
            _ => {
                return Err(ParsingError::expected_next_token(
                    TokenKind::LBrace.to_string(),
                    self.current_token.kind.to_string(),
                    self.position,
                ))
            }
        };

        Ok(FunctionDefinition {
            name: Identifier {
                identifier: ident,
                position: self.position,
            },
            parameters,
            return_type,
            body,
            position: self.position,
        })
    }

    fn parse_external_function_declaration(&mut self) -> ParseResult<ExternalFunctionDeclaration> {
        self.next_token();

        self.expect_token(&TokenKind::Function)?;

        let ident = ident_token_to_string! { self };
        self.next_token();

        self.expect_token(&TokenKind::LParen)?;

        let mut parameters = Vec::new();

        while self.current_token.kind != TokenKind::RParen {
            parameters.push(self.parse_ty()?);

            if self.current_token.kind == TokenKind::RParen {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        self.expect_token(&TokenKind::RParen)?;
        self.expect_token(&TokenKind::Arrow)?;

        let return_type = self.parse_ty()?;

        expect_semicolon_without_next_token! { self }

        Ok(ExternalFunctionDeclaration {
            name: Identifier {
                identifier: ident,
                position: self.position,
            },
            parameters,
            return_type,
            position: self.position,
        })
    }

    fn parse_return_statement(&mut self) -> ParseResult<ReturnStatement> {
        self.next_token();

        if let Ok(expression) = self.parse_expression(&Priority::Lowest) {
            return if self.peek_token(&TokenKind::Semicolon) {
                self.next_token();

                Ok(ReturnStatement {
                    value: expression,
                    position: self.position,
                })
            } else {
                Err(ParsingError::expected_next_token(
                    TokenKind::Semicolon.to_string(),
                    self.current_token.kind.to_string(),
                    self.position,
                ))
            };
        }

        Err(ParsingError::unexpected_token(
            self.current_token.kind.to_string(),
            self.position,
        ))
    }

    fn parse_type_statement(&mut self) -> ParseResult<TypeDeclaration> {
        self.next_token();

        let ident = ident_token_to_string! { self };
        self.next_token();

        self.expect_token(&TokenKind::Assign)?;

        let ty = self.parse_ty()?;

        expect_semicolon_without_next_token! { self }

        Ok(TypeDeclaration {
            name: Identifier {
                identifier: ident,
                position: self.position,
            },
            ty,
            position: self.position,
        })
    }

    fn parse_declare_statement(&mut self) -> ParseResult<Declaration> {
        self.next_token();

        let ident = ident_token_to_string! { self };
        self.next_token();

        self.expect_token(&TokenKind::Assign)?;

        let ty = self.parse_ty()?;
        expect_semicolon_without_next_token! { self }

        Ok(Declaration {
            name: Identifier {
                identifier: ident,
                position: self.position,
            },
            ty,
            position: self.position,
        })
    }

    fn parse_struct_declaration(&mut self) -> ParseResult<StructDeclaration> {
        self.next_token();

        let ident = ident_token_to_string! { self };
        self.next_token();

        self.expect_token(&TokenKind::LBrace)?;

        let mut fields = BTreeMap::new();

        while self.current_token.kind != TokenKind::RBrace {
            let key = ident_token_to_string! { self };
            self.next_token();

            self.expect_token(&TokenKind::Colon)?;

            let ty = self.parse_ty()?;
            fields.insert(
                key.clone(),
                FieldTy {
                    ty,
                    position: self.position,
                },
            );

            if self.current_token.kind == TokenKind::RBrace {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        self.expect_token(&TokenKind::RBrace)?;

        Ok(StructDeclaration {
            name: Identifier {
                identifier: ident,
                position: self.position,
            },
            fields,
            position: self.position,
        })
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let expression = self.parse_expression(&Priority::Lowest)?;
        self.next_token();

        Ok(Statement::Expression(expression))
    }

    fn parse_expression(&mut self, priority: &Priority) -> ParseResult<Expression> {
        let left_expression = match self.current_token.kind.clone() {
            TokenKind::IDENT(value) => {
                Some(Ok(Expression::Literal(Literal::Identifier(Identifier {
                    identifier: value.to_string(),
                    position: self.position,
                }))))
            }
            TokenKind::Int(value) => Some(Ok(Expression::Literal(Literal::Int(IntLiteral {
                value,
                position: self.position,
            })))),
            TokenKind::Float(value) => {
                Some(Ok(Expression::Literal(Literal::Float(FloatLiteral {
                    value,
                    position: self.position,
                }))))
            }
            TokenKind::String(value) => {
                Some(Ok(Expression::Literal(Literal::String(StringLiteral {
                    value: value.to_string(),
                    position: self.position,
                }))))
            }
            TokenKind::Boolean(value) => {
                Some(Ok(Expression::Literal(Literal::Boolean(BooleanLiteral {
                    value,
                    position: self.position,
                }))))
            }
            TokenKind::Bang | TokenKind::Minus => {
                let operator: UnaryOperator = self.current_token.kind.clone().into();
                self.next_token();

                Some(Ok(Expression::Unary(UnaryExpression {
                    operator,
                    expression: Box::new(self.parse_expression(&Priority::Prefix)?),
                    position: self.position,
                })))
            }
            TokenKind::LParen => {
                self.next_token();

                let expression = self.parse_expression(&Priority::Lowest);
                self.next_token();

                if self.current_token.kind != TokenKind::RParen {
                    return Err(ParsingError::expected_next_token(
                        TokenKind::RParen.to_string(),
                        self.current_token.kind.to_string(),
                        self.position,
                    ));
                }

                Some(expression)
            }
            TokenKind::LBrace => Some(Ok(Expression::Block(self.parse_block_expression()?))),
            TokenKind::LBracket => Some(Ok(Expression::Literal(Literal::Array(
                self.parse_array_literal()?,
            )))),
            TokenKind::Struct => Some(Ok(Expression::Literal(Literal::Struct(
                self.parse_struct_literal()?,
            )))),
            TokenKind::If => Some(Ok(Expression::If(self.parse_if_expression()?))),
            TokenKind::Typeof => {
                self.next_token();

                Some(Ok(Expression::Typeof(TypeofExpression {
                    expression: Box::new(self.parse_expression(&Priority::Lowest)?),
                    position: self.position,
                })))
            }
            TokenKind::Sizeof => {
                self.next_token();

                Some(Ok(Expression::Sizeof(SizeofExpression {
                    expression: Box::new(self.parse_expression(&Priority::Lowest)?),
                    position: self.position,
                })))
            }
            _ => None,
        };

        if left_expression.is_none() && self.current_token.kind != TokenKind::Semicolon {
            return Err(ParsingError::unexpected_token(
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        let mut left_expression = left_expression.ok_or_else(|| {
            ParsingError::unexpected_token(self.current_token.kind.to_string(), self.position)
        })?;

        if self.peek_token(&TokenKind::Assign) {
            let identifier = Identifier {
                identifier: ident_token_to_string! { self },
                position: self.position,
            };
            self.next_token();
            self.next_token();

            let expression = self.parse_expression(&Priority::Lowest)?;
            return Ok(Expression::Assign(AssignExpression {
                name: identifier,
                value: Box::new(expression),
                position: self.position,
            }));
        }

        while !self.peek_token(&TokenKind::Semicolon) && priority < &self.peek_priority() {
            self.next_token();

            left_expression = match self.current_token.kind {
                TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Dot
                | TokenKind::Slash
                | TokenKind::Asterisk
                | TokenKind::Percent
                | TokenKind::EQ
                | TokenKind::NEQ
                | TokenKind::LT
                | TokenKind::GT
                | TokenKind::LTE
                | TokenKind::GTE => {
                    let operator: BinaryOperator = self.current_token.kind.clone().into();

                    let priority = self.current_priority();
                    self.next_token();
                    let right = Box::new(self.parse_expression(&priority)?);

                    Ok(Expression::Binary(BinaryExpression {
                        left: Box::new(left_expression?),
                        operator,
                        right,
                        position: self.position,
                    }))
                }
                TokenKind::LParen => {
                    self.next_token();

                    let mut arguments = Vec::new();

                    if self.current_token.kind != TokenKind::RParen {
                        arguments.push(self.parse_expression(&Priority::Lowest)?);
                        self.next_token();

                        if self.current_token.kind == TokenKind::Comma {
                            self.next_token();
                        }

                        while self.current_token.kind != TokenKind::RParen {
                            arguments.push(self.parse_expression(&Priority::Lowest)?);
                            self.next_token();

                            if self.current_token.kind == TokenKind::RParen {
                                break;
                            }

                            self.expect_token(&TokenKind::Comma)?;
                        }

                        if self.current_token.kind != TokenKind::RParen {
                            return Err(ParsingError::expected_next_token(
                                TokenKind::RParen.to_string(),
                                self.current_token.kind.to_string(),
                                self.position,
                            ));
                        }
                    }

                    Ok(Expression::Call(CallExpression {
                        function: Box::new(left_expression?),
                        arguments,
                        position: self.position,
                    }))
                }
                TokenKind::LBracket => {
                    self.next_token();

                    let index = self.parse_expression(&Priority::Lowest)?;
                    self.next_token();

                    if self.current_token.kind != TokenKind::RBracket {
                        return Err(ParsingError::expected_next_token(
                            TokenKind::RBracket.to_string(),
                            self.current_token.kind.to_string(),
                            self.position,
                        ));
                    }

                    Ok(Expression::Index(IndexExpression {
                        left: Box::new(left_expression?),
                        index: Box::new(index),
                        position: self.position,
                    }))
                }
                _ => Err(ParsingError::unexpected_token(
                    self.current_token.kind.to_string(),
                    self.position,
                )),
            };
        }

        left_expression
    }

    fn parse_block_expression(&mut self) -> ParseResult<BlockExpression> {
        self.next_token();

        let mut statements = Vec::new();

        while self.current_token.kind != TokenKind::RBrace {
            statements.push(self.parse_statement()?);
            self.next_token();
        }

        Ok(BlockExpression {
            statements,
            position: self.position,
        })
    }

    fn parse_array_literal(&mut self) -> ParseResult<ArrayLiteral> {
        self.next_token();

        let mut elements = Vec::new();

        if self.current_token.kind == TokenKind::RBracket {
            return Ok(ArrayLiteral {
                elements,
                position: self.position,
            });
        }

        while self.current_token.kind != TokenKind::RBrace {
            elements.push(self.parse_expression(&Priority::Lowest)?);
            self.next_token();

            if self.current_token.kind == TokenKind::RBracket {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        if self.current_token.kind != TokenKind::RBracket {
            return Err(ParsingError::expected_next_token(
                TokenKind::RBracket.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        Ok(ArrayLiteral {
            elements,
            position: self.position,
        })
    }

    fn parse_struct_literal(&mut self) -> ParseResult<StructLiteral> {
        self.next_token();

        let identifier_position = self.position;
        let identifier = ident_token_to_string! { self };

        self.next_token();
        self.expect_token(&TokenKind::LBrace)?;

        let mut fields = BTreeMap::new();

        while self.current_token.kind != TokenKind::RBrace {
            let key = ident_token_to_string! { self };
            self.next_token();

            self.expect_token(&TokenKind::Colon)?;

            fields.insert(key.clone(), self.parse_expression(&Priority::Lowest)?);
            self.next_token();

            if self.current_token.kind == TokenKind::RBrace {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        if self.current_token.kind != TokenKind::RBrace {
            return Err(ParsingError::expected_next_token(
                TokenKind::RBrace.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        Ok(StructLiteral {
            name: Identifier {
                identifier,
                position: identifier_position,
            },
            fields,
            position: self.position,
        })
    }

    fn parse_if_expression(&mut self) -> ParseResult<IfExpression> {
        self.next_token();

        let condition = self.parse_expression(&Priority::Lowest)?;
        self.next_token();

        if self.current_token.kind != TokenKind::LBrace {
            return Err(ParsingError::expected_next_token(
                TokenKind::LBrace.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        let consequence = self.parse_block_expression()?;

        let alternative = if self.peek_token.kind == TokenKind::Else {
            self.next_token();
            self.next_token();

            if self.current_token.kind == TokenKind::If {
                Some(BlockExpression {
                    statements: vec![Statement::Return(ReturnStatement {
                        value: Expression::If(self.parse_if_expression()?),
                        position: self.position,
                    })],
                    position: self.position,
                })
            } else {
                if self.current_token.kind != TokenKind::LBrace {
                    return Err(ParsingError::expected_next_token(
                        TokenKind::LBrace.to_string(),
                        self.current_token.kind.to_string(),
                        self.position,
                    ));
                }

                Some(self.parse_block_expression()?)
            }
        } else {
            None
        };

        Ok(IfExpression {
            condition: Box::new(condition),
            consequence,
            alternative,
            position: self.position,
        })
    }

    fn parse_ty(&mut self) -> ParseResult<AstType> {
        let position = self.position;

        let result = self.parse_ty_kind();
        self.next_token();

        result.map(|kind| AstType { kind, position })
    }

    fn parse_ty_kind(&mut self) -> ParseResult<AstTypeKind> {
        let mut ty = match self.current_token.kind {
            TokenKind::IntType => Ok(AstTypeKind::Int),
            TokenKind::FloatType => Ok(AstTypeKind::Float),
            TokenKind::StringType => Ok(AstTypeKind::String),
            TokenKind::BooleanType => Ok(AstTypeKind::Boolean),
            TokenKind::Function => todo!(),
            TokenKind::IDENT(ref ident) => Ok(AstTypeKind::Named(Identifier {
                identifier: ident.to_string(),
                position: self.position,
            })),
            _ => Err(ParsingError::unexpected_token(
                self.current_token.kind.to_string(),
                self.position,
            )),
        };

        if self.peek_token(&TokenKind::LBracket) {
            self.next_token();
            self.next_token();

            let mut size = None;

            if self.current_token.kind != TokenKind::RBracket {
                let expression = self.parse_expression(&Priority::Lowest)?;
                self.next_token();

                if self.current_token.kind != TokenKind::RBracket {
                    return Err(ParsingError::expected_next_token(
                        TokenKind::RBracket.to_string(),
                        self.current_token.kind.to_string(),
                        self.position,
                    ));
                }

                match expression {
                    Expression::Literal(Literal::Int(int)) => {
                        size = Some(int.value as usize);
                    }
                    _ => {
                        return Err(ParsingError::expected_next_token(
                            "Int".to_string(),
                            self.current_token.kind.to_string(),
                            self.position,
                        ));
                    }
                }
            }

            ty = Ok(AstTypeKind::Array(AstArrayTypeKind {
                ty: Box::new(AstType {
                    kind: ty?,
                    position: self.position,
                }),
                len: size,
                position: self.position,
            }));
        }

        ty
    }
}
