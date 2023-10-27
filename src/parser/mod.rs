pub mod error;
pub mod helpers;

use self::error::{ParseResult, ParsingError};
use crate::{
    ast::*,
    ident_token_to_string,
    tokenizer::{Lexer, Token, TokenKind},
};

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

        self.position = Position(self.current_token.position.0, self.current_token.position.1);
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

    fn get_priority(&self, token_type: &TokenKind) -> Priority {
        match token_type {
            TokenKind::Dot | TokenKind::Arrow => Priority::Dot,
            TokenKind::Assign | TokenKind::EQ | TokenKind::NEQ => Priority::Equals,
            TokenKind::Plus | TokenKind::Minus => Priority::Sum,
            TokenKind::Slash | TokenKind::Asterisk => Priority::Product,
            TokenKind::LT | TokenKind::GT | TokenKind::LTE | TokenKind::GTE => {
                Priority::LessGreater
            }
            TokenKind::LParen => Priority::Call,
            TokenKind::LBracket => Priority::Index,
            _ => Priority::Lowest,
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
                Ok(statement) => program.push(statement),
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
            TokenKind::Let => Statement::LetStatement(self.parse_let_statement(false)?),
            TokenKind::Mut => Statement::LetStatement(self.parse_let_statement(true)?),
            TokenKind::Function => {
                Statement::FunctionDeclaration(self.parse_function_declaration()?)
            }
            TokenKind::Extern => {
                Statement::ExternFunctionDeclaration(self.parse_extern_function_declaration()?)
            }
            TokenKind::Return => Statement::ReturnStatement(self.parse_return_statement()?),
            TokenKind::Type => Statement::TypeStatement(self.parse_type_statement()?),
            TokenKind::Declare => Statement::DeclareStatement(self.parse_declare_statement()?),
            TokenKind::Struct => Statement::StructStatement(self.parse_struct_statement()?),
            _ => self.parse_expression_statement()?,
        })
    }

    fn parse_let_statement(&mut self, is_mutable: bool) -> ParseResult<LetStatement> {
        self.next_token();

        let ident = Identifier {
            value: ident_token_to_string! { self },
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
            identifier: ident,
            ty,
            value: Some(expression),
            is_mutable,
            position: self.position,
        })
    }

    fn parse_function_declaration(&mut self) -> ParseResult<FunctionDeclaration> {
        self.next_token();

        let ident = ident_token_to_string! { self };

        Ok(FunctionDeclaration {
            identifier: Identifier {
                value: ident,
                position: self.position,
            },
            function: self.parse_function_literal()?,
        })
    }

    fn parse_extern_function_declaration(&mut self) -> ParseResult<ExternFunctionDeclaration> {
        self.next_token();

        self.expect_token(&TokenKind::Function)?;

        let ident = ident_token_to_string! { self };
        self.next_token();

        let generics = if self.current_token.kind == TokenKind::LT {
            let result = self.parse_generic_identifier()?;
            self.next_token();
            Some(result)
        } else {
            None
        };

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

        let ret = self.parse_ty()?;

        if self.current_token.kind != TokenKind::Semicolon {
            return Err(ParsingError::expected_next_token(
                TokenKind::Semicolon.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        Ok(ExternFunctionDeclaration {
            identifier: Identifier {
                value: ident,
                position: self.position,
            },
            parameters,
            ret,
            generics,
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

    fn parse_type_statement(&mut self) -> ParseResult<TypeStatement> {
        self.next_token();

        let ident = ident_token_to_string! { self };
        self.next_token();

        let generics = if self.current_token.kind == TokenKind::LT {
            let generic = self.parse_generic_identifier()?;
            self.next_token();
            Some(generic)
        } else {
            None
        };

        self.expect_token(&TokenKind::Assign)?;

        let ty = self.parse_ty()?;

        if self.current_token.kind != TokenKind::Semicolon {
            return Err(ParsingError::expected_next_token(
                TokenKind::Semicolon.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        Ok(TypeStatement {
            identifier: Identifier {
                value: ident,
                position: self.position,
            },
            ty,
            generics,
            position: self.position,
        })
    }

    fn parse_declare_statement(&mut self) -> ParseResult<DeclareStatement> {
        self.next_token();

        let ident = ident_token_to_string! { self };
        self.next_token();

        self.expect_token(&TokenKind::Assign)?;

        let ty = self.parse_ty()?;

        if self.current_token.kind != TokenKind::Semicolon {
            return Err(ParsingError::expected_next_token(
                TokenKind::Semicolon.to_string(),
                self.current_token.kind.to_string(),
                self.position,
            ));
        }

        Ok(DeclareStatement {
            identifier: Identifier {
                value: ident,
                position: self.position,
            },
            ty,
            position: self.position,
        })
    }

    fn parse_struct_statement(&mut self) -> ParseResult<StructStatement> {
        self.next_token();

        let ident = ident_token_to_string! { self };
        self.next_token();

        let generics = if self.current_token.kind == TokenKind::LT {
            let generic = self.parse_generic_identifier()?;
            self.next_token();
            Some(generic)
        } else {
            None
        };

        self.expect_token(&TokenKind::LBrace)?;

        let mut fields = Vec::new();

        while self.current_token.kind != TokenKind::RBrace {
            let key = Identifier {
                value: ident_token_to_string! { self },
                position: self.position,
            };
            self.next_token();

            self.expect_token(&TokenKind::Colon)?;

            let value = self.parse_ty()?;

            fields.push(StructField {
                identifier: key,
                ty: value,
                position: self.position,
            });

            if self.current_token.kind == TokenKind::RBrace {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        self.expect_token(&TokenKind::RBrace)?;

        Ok(StructStatement {
            identifier: Identifier {
                value: ident,
                position: self.position,
            },
            generics,
            fields,
            position: self.position,
        })
    }

    fn parse_expression_statement(&mut self) -> ParseResult<Statement> {
        let expression = self.parse_expression(&Priority::Lowest)?;
        self.next_token();

        Ok(Statement::ExpressionStatement(ExpressionStatement {
            expression,
            position: self.position,
        }))
    }

    fn parse_expression(&mut self, priority: &Priority) -> ParseResult<Expression> {
        let left_expression = match self.current_token.kind.clone() {
            TokenKind::IDENT(value) => {
                Some(Ok(Expression::Literal(Literal::Identifier(Identifier {
                    value: value.to_string(),
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
                let operator: PrefixOperator = self.current_token.kind.clone().into();
                self.next_token();

                Some(Ok(Expression::PrefixExpression(PrefixExpression {
                    operator,
                    right: Box::new(self.parse_expression(&Priority::Prefix)?),
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
            TokenKind::LBrace => Some(Ok(Expression::BlockExpression(
                self.parse_block_expression()?,
            ))),
            TokenKind::LBracket => Some(Ok(Expression::Literal(Literal::Array(
                self.parse_array_literal()?,
            )))),
            TokenKind::Function => Some(Ok(Expression::Literal(Literal::Function(
                self.parse_function_literal()?,
            )))),
            TokenKind::Struct => Some(Ok(Expression::Literal(Literal::Struct(
                self.parse_struct_literal()?,
            )))),
            TokenKind::If => Some(Ok(Expression::IfExpression(self.parse_if_expression()?))),
            TokenKind::Typeof => {
                self.next_token();

                Some(Ok(Expression::TypeofExpression(TypeofExpression {
                    expression: Box::new(self.parse_expression(&Priority::Lowest)?),
                    position: self.position,
                })))
            }
            TokenKind::Debug => {
                self.next_token();

                Some(Ok(Expression::Debug(
                    Box::new(self.parse_expression(&Priority::Lowest)?),
                    self.position,
                )))
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
                value: ident_token_to_string! { self },
                position: self.position,
            };
            self.next_token();
            self.next_token();

            let expression = self.parse_expression(&Priority::Lowest)?;
            return Ok(Expression::AssignmentExpression(AssignmentExpression {
                identifier,
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
                    let operator: InfixOperator = self.current_token.kind.clone().into();

                    let priority = self.current_priority();
                    self.next_token();
                    let right = Box::new(self.parse_expression(&priority)?);

                    Ok(Expression::InfixExpression(InfixExpression {
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

                    Ok(Expression::CallExpression(CallExpression {
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

                    Ok(Expression::IndexExpression(IndexExpression {
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
        let identifier = ident_token_to_string! { self };
        self.next_token();

        self.expect_token(&TokenKind::LBrace)?;

        let mut fields = Vec::new();

        while self.current_token.kind != TokenKind::RBrace {
            let key = Identifier {
                value: ident_token_to_string! { self },
                position: self.position,
            };
            self.next_token();

            self.expect_token(&TokenKind::Colon)?;

            let value = self.parse_expression(&Priority::Lowest)?;
            self.next_token();

            fields.push((key, value));

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
            identifier: Identifier {
                value: identifier,
                position: self.position,
            },
            fields,
            position: self.position,
        })
    }

    fn parse_function_literal(&mut self) -> ParseResult<FunctionLiteral> {
        self.next_token();

        let generics = if self.current_token.kind == TokenKind::LT {
            let generic = self.parse_generic_identifier()?;
            self.next_token();
            Some(generic)
        } else {
            None
        };

        self.expect_token(&TokenKind::LParen)?;

        let mut parameters = Vec::new();

        while self.current_token.kind != TokenKind::RParen {
            if let TokenKind::IDENT(identifier) = self.current_token.kind.clone() {
                self.next_token();
                self.expect_token(&TokenKind::Colon)?;

                let ty = self.parse_ty()?;

                parameters.push(Parameter {
                    identifier: Identifier {
                        value: identifier.to_string(),
                        position: self.position,
                    },
                    ty,
                    position: self.position,
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

        let ret = self.parse_ty()?;

        let body = match self.current_token.kind {
            TokenKind::LBrace => self.parse_block_expression()?,
            TokenKind::DoubleArrow => {
                self.next_token();

                BlockExpression {
                    statements: vec![Statement::ReturnStatement(ReturnStatement {
                        value: self.parse_expression(&Priority::Lowest)?,
                        position: self.position,
                    })],
                    position: self.position,
                }
            }
            _ => {
                return Err(ParsingError::expected_next_token(
                    TokenKind::LBrace.to_string(),
                    self.current_token.kind.to_string(),
                    self.position,
                ))
            }
        };

        Ok(FunctionLiteral {
            generics,
            parameters,
            ret,
            body,
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
                Some(Box::new(BlockExpression {
                    statements: vec![Statement::ReturnStatement(ReturnStatement {
                        value: Expression::IfExpression(self.parse_if_expression()?),
                        position: self.position,
                    })],
                    position: self.position,
                }))
            } else {
                if self.current_token.kind != TokenKind::LBrace {
                    return Err(ParsingError::expected_next_token(
                        TokenKind::LBrace.to_string(),
                        self.current_token.kind.to_string(),
                        self.position,
                    ));
                }

                Some(Box::new(self.parse_block_expression()?))
            }
        } else {
            None
        };

        Ok(IfExpression {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
            position: self.position,
        })
    }

    fn parse_ty(&mut self) -> ParseResult<Ty> {
        let position = self.position;

        let result = self.parse_ty_without_next();
        self.next_token();

        result.map(|ty| Ty { kind: ty, position })
    }

    fn parse_ty_without_next(&mut self) -> ParseResult<TyKind> {
        let mut ty: Result<TyKind, ParsingError> = match self.current_token.kind {
            TokenKind::IntType => Ok(TyKind::Int),
            TokenKind::FloatType => Ok(TyKind::Float),
            TokenKind::StringType => Ok(TyKind::String),
            TokenKind::BooleanType => Ok(TyKind::Boolean),
            TokenKind::Function => Ok(TyKind::Fn(self.parse_function_type()?)),
            TokenKind::IDENT(ref ident) => Ok(TyKind::Custom(ident.to_string())),
            _ => Err(ParsingError::unexpected_token(
                self.current_token.kind.to_string(),
                self.position,
            )),
        };

        if self.peek_token(&TokenKind::LT) {
            let generics = self.parse_generic()?;

            ty = Ok(TyKind::Generic(generics));
        }

        if self.peek_token(&TokenKind::LBracket) {
            self.next_token();
            self.next_token();

            ty = Ok(TyKind::Array(Box::new(Ty {
                kind: ty?,
                position: self.position,
            })));

            if self.current_token.kind != TokenKind::RBracket {
                return Err(ParsingError::expected_next_token(
                    TokenKind::RBracket.to_string(),
                    self.current_token.kind.to_string(),
                    self.position,
                ));
            }
        }

        ty
    }

    fn parse_function_type(&mut self) -> ParseResult<FunctionType> {
        self.next_token();

        let generics = if self.current_token.kind == TokenKind::LT {
            let generic = self.parse_generic_identifier()?;
            self.next_token();
            Some(generic)
        } else {
            None
        };

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

        let return_type = self.parse_ty_without_next()?;

        Ok(FunctionType {
            generics,
            parameters,
            ret: Box::new(Ty {
                kind: return_type,
                position: self.position,
            }),
            position: self.position,
        })
    }

    fn parse_generic(&mut self) -> ParseResult<Generic> {
        let ident = ident_token_to_string! { self };
        self.next_token();

        let mut generics = Vec::new();

        self.expect_token(&TokenKind::LT)?;

        while self.current_token.kind != TokenKind::GT {
            let ty = self.parse_ty()?;

            generics.push(ty);

            if self.current_token.kind == TokenKind::GT {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        Ok(Generic::new(
            Ty {
                kind: TyKind::Custom(ident),
                position: self.position,
            },
            generics,
        ))
    }

    fn parse_generic_identifier(&mut self) -> ParseResult<IdentifierGeneric> {
        let mut generics = Vec::new();

        self.expect_token(&TokenKind::LT)?;

        while self.current_token.kind != TokenKind::GT {
            let ident = ident_token_to_string! { self };
            self.next_token();

            generics.push(Identifier {
                value: ident,
                position: self.position,
            });

            if self.current_token.kind == TokenKind::GT {
                break;
            }

            self.expect_token(&TokenKind::Comma)?;
        }

        Ok(generics)
    }
}
