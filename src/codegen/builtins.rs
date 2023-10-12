use inkwell::{
    context::Context,
    module::{Linkage, Module},
    types::{BasicMetadataTypeEnum, FunctionType},
    AddressSpace,
};

pub trait Builtin {
    fn name(&self) -> &str;
    fn ty<'a>(&self, context: &'a Context) -> FunctionType<'a>;
}

pub fn add_builtins<'a>(module: &Module<'a>, context: &'a Context) {
    macro_rules! add_function {
        ($($fn:ident)*) => {
            $(
                module.add_function($fn.name(), $fn.ty(context), Some(Linkage::External));
            )*
        }
    }

    add_function! { Printf Print PrintStr PrintArr }
}

struct Printf;

impl Builtin for Printf {
    fn name(&self) -> &str {
        "printf"
    }

    fn ty<'a>(&self, context: &'a Context) -> FunctionType<'a> {
        context.i64_type().fn_type(
            vec![BasicMetadataTypeEnum::PointerType(
                context.i8_type().ptr_type(AddressSpace::from(0)),
            )]
            .as_slice(),
            true,
        )
    }
}

struct Print;

impl Builtin for Print {
    fn name(&self) -> &str {
        "print"
    }

    fn ty<'a>(&self, context: &'a Context) -> FunctionType<'a> {
        context.i64_type().fn_type(
            vec![BasicMetadataTypeEnum::IntType(context.i64_type())].as_slice(),
            true,
        )
    }
}

struct PrintStr;

impl Builtin for PrintStr {
    fn name(&self) -> &str {
        "print_str"
    }

    fn ty<'a>(&self, context: &'a Context) -> FunctionType<'a> {
        context.i8_type().ptr_type(AddressSpace::from(0)).fn_type(
            vec![BasicMetadataTypeEnum::PointerType(
                context.i8_type().ptr_type(AddressSpace::from(0)),
            )]
            .as_slice(),
            true,
        )
    }
}

struct PrintArr;

impl Builtin for PrintArr {
    fn name(&self) -> &str {
        "print_array"
    }

    fn ty<'a>(&self, context: &'a Context) -> FunctionType<'a> {
        context.i64_type().fn_type(
            vec![
                BasicMetadataTypeEnum::PointerType(
                    context.i64_type().ptr_type(AddressSpace::from(0)),
                ),
                BasicMetadataTypeEnum::IntType(context.i64_type()),
            ]
            .as_slice(),
            true,
        )
    }
}
