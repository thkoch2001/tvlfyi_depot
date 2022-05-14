pub(super) mod value {
    use inkwell::values::{AnyValueEnum, BasicMetadataValueEnum};

    #[track_caller]
    pub fn any_to_basic_metadata(val: AnyValueEnum) -> BasicMetadataValueEnum {
        match val {
            AnyValueEnum::ArrayValue(v) => BasicMetadataValueEnum::ArrayValue(v),
            AnyValueEnum::IntValue(v) => BasicMetadataValueEnum::IntValue(v),
            AnyValueEnum::FloatValue(v) => BasicMetadataValueEnum::FloatValue(v),
            AnyValueEnum::PointerValue(v) => BasicMetadataValueEnum::PointerValue(v),
            AnyValueEnum::StructValue(v) => BasicMetadataValueEnum::StructValue(v),
            AnyValueEnum::VectorValue(v) => BasicMetadataValueEnum::VectorValue(v),
            AnyValueEnum::PhiValue(_)
            | AnyValueEnum::FunctionValue(_)
            | AnyValueEnum::InstructionValue(_) => {
                panic!("AnyValueEnum -> BasicMetadataValueEnum conversion failed")
            }
        }
    }
}

pub(super) mod type_ {
    use inkwell::types::{BasicMetadataTypeEnum, BasicTypeEnum};

    #[track_caller]
    pub fn basic_to_basic_metadata(ty: BasicTypeEnum) -> BasicMetadataTypeEnum {
        match ty {
            BasicTypeEnum::ArrayType(v) => BasicMetadataTypeEnum::ArrayType(v),
            BasicTypeEnum::FloatType(v) => BasicMetadataTypeEnum::FloatType(v),
            BasicTypeEnum::IntType(v) => BasicMetadataTypeEnum::IntType(v),
            BasicTypeEnum::PointerType(v) => BasicMetadataTypeEnum::PointerType(v),
            BasicTypeEnum::StructType(v) => BasicMetadataTypeEnum::StructType(v),
            BasicTypeEnum::VectorType(v) => BasicMetadataTypeEnum::VectorType(v),
        }
    }
}
