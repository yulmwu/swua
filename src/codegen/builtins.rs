use inkwell::{
    context::Context,
    module::{Linkage, Module},
    types::BasicMetadataTypeEnum,
    AddressSpace,
};

pub fn add_builtins<'a>(module: &Module<'a>, context: &'a Context) {
    module.add_function(
        "printf",
        context.i64_type().fn_type(
            vec![BasicMetadataTypeEnum::PointerType(
                context.i8_type().ptr_type(AddressSpace::from(0)),
            )]
            .as_slice(),
            true,
        ),
        Some(Linkage::External),
    );

    module.add_function(
        "print",
        context.i64_type().fn_type(
            vec![BasicMetadataTypeEnum::IntType(context.i64_type())].as_slice(),
            true,
        ),
        Some(Linkage::External),
    );

    module.add_function(
        "print_str",
        context.i8_type().ptr_type(AddressSpace::from(0)).fn_type(
            vec![BasicMetadataTypeEnum::PointerType(
                context.i8_type().ptr_type(AddressSpace::from(0)),
            )]
            .as_slice(),
            true,
        ),
        Some(Linkage::External),
    );
}
