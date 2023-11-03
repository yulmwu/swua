#[macro_export]
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
