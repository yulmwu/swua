use crate::ast::TyKind;
use inkwell::values::BasicValueEnum;

#[derive(Clone, Debug)]
pub struct Value<'ctx> {
    pub value: BasicValueEnum<'ctx>,
    pub ty: TyKind,
}

impl<'ctx> Value<'ctx> {
    pub fn new(value: BasicValueEnum<'ctx>, ty: TyKind) -> Self {
        Self { value, ty }
    }
}
