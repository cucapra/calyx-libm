use calyx_libm_utils::Format;

use super::{Import, ImportSet, Primitive, primitives};

use NumericType::{SFixed, SInt, UFixed, UInt};

pub struct Importer {
    imports: ImportSet,
}

impl Importer {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Importer {
        let mut imports = ImportSet::new();

        imports.insert(Import::Core);

        Importer { imports }
    }

    pub fn import(&mut self, file: Import) {
        self.imports.insert(file);
    }

    pub fn into_imports(self) -> ImportSet {
        self.imports
    }

    pub fn not(&mut self) -> &'static Primitive<'static> {
        &primitives::core::STD_NOT
    }

    pub fn and(&mut self) -> &'static Primitive<'static> {
        &primitives::core::STD_AND
    }

    pub fn or(&mut self) -> &'static Primitive<'static> {
        &primitives::core::STD_OR
    }

    pub fn add(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::compile::STD_ADD,
            SInt => &primitives::binary_operators::STD_SADD,
            UFixed => &primitives::binary_operators::STD_FP_ADD,
            SFixed => &primitives::binary_operators::STD_FP_SADD,
        };

        self.import(primitive.import);

        primitive
    }

    pub fn sub(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::core::STD_SUB,
            SInt => &primitives::binary_operators::STD_SSUB,
            UFixed => &primitives::binary_operators::STD_FP_SUB,
            SFixed => &primitives::binary_operators::STD_FP_SSUB,
        };

        self.import(primitive.import);

        primitive
    }

    pub fn mul(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::binary_operators::STD_MULT_PIPE,
            SInt => &primitives::binary_operators::STD_SMULT_PIPE,
            UFixed => &primitives::binary_operators::STD_FP_MULT_PIPE,
            SFixed => &primitives::binary_operators::STD_FP_SMULT_PIPE,
        };

        self.import(primitive.import);

        primitive
    }

    pub fn div(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::binary_operators::STD_DIV_PIPE_QUOTIENT,
            SInt => &primitives::binary_operators::STD_SDIV_PIPE_QUOTIENT,
            UFixed => &primitives::binary_operators::STD_FP_DIV_PIPE_QUOTIENT,
            SFixed => &primitives::binary_operators::STD_FP_SDIV_PIPE_QUOTIENT,
        };

        self.import(primitive.import);

        primitive
    }

    #[allow(dead_code)]
    pub fn rem(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::binary_operators::STD_DIV_PIPE_REMAINDER,
            SInt => &primitives::binary_operators::STD_SDIV_PIPE_REMAINDER,
            UFixed => &primitives::binary_operators::STD_FP_DIV_PIPE_REMAINDER,
            SFixed => &primitives::binary_operators::STD_FP_SDIV_PIPE_REMAINDER,
        };

        self.import(primitive.import);

        primitive
    }

    pub fn neg(&mut self, _format: &Format) -> &'static Primitive<'static> {
        let primitive = &primitives::numbers::NUM_SNEG;

        self.import(primitive.import);

        primitive
    }

    pub fn abs(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt | UFixed => &primitives::compile::STD_WIRE,
            SInt | SFixed => &primitives::numbers::NUM_SABS,
        };

        self.import(primitive.import);

        primitive
    }

    pub fn gt(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::core::STD_GT,
            SInt => &primitives::binary_operators::STD_SGT,
            UFixed => &primitives::binary_operators::STD_FP_GT,
            SFixed => &primitives::binary_operators::STD_FP_SGT,
        };

        self.import(primitive.import);

        primitive
    }

    pub fn lt(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::core::STD_LT,
            SInt => &primitives::binary_operators::STD_SLT,
            UFixed => &primitives::core::STD_LT, // no STD_FP_LT
            SFixed => &primitives::binary_operators::STD_FP_SLT,
        };

        self.import(primitive.import);

        primitive
    }

    pub fn eq(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::core::STD_EQ,
            SInt => &primitives::binary_operators::STD_SEQ,
            UFixed => &primitives::core::STD_EQ, // no STD_FP_EQ
            SFixed => &primitives::binary_operators::STD_SEQ, // no STD_FP_SEQ
        };

        self.import(primitive.import);

        primitive
    }

    pub fn neq(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::core::STD_NEQ,
            SInt => &primitives::binary_operators::STD_SNEQ,
            UFixed => &primitives::core::STD_NEQ, // no STD_FP_NEQ
            SFixed => &primitives::binary_operators::STD_SNEQ, // no STD_FP_SNEQ
        };

        self.import(primitive.import);

        primitive
    }

    pub fn ge(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::core::STD_GE,
            SInt => &primitives::binary_operators::STD_SGE,
            UFixed => &primitives::core::STD_GE, // no STD_FP_GE
            SFixed => &primitives::binary_operators::STD_SGE, // no STD_FP_SGE
        };

        self.import(primitive.import);

        primitive
    }

    pub fn le(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt => &primitives::core::STD_LE,
            SInt => &primitives::binary_operators::STD_SLE,
            UFixed => &primitives::core::STD_LE, // no STD_FP_LE
            SFixed => &primitives::binary_operators::STD_SLE, // no STD_FP_SLE
        };

        self.import(primitive.import);

        primitive
    }

    pub fn sqrt(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt | SInt => &primitives::math::STD_SQRT,
            UFixed | SFixed => &primitives::math::STD_FP_SQRT,
        };

        self.import(primitive.import);

        primitive
    }

    pub fn min(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt | UFixed => &primitives::numbers::NUM_MIN,
            SInt | SFixed => &primitives::numbers::NUM_SMIN,
        };

        self.import(primitive.import);

        primitive
    }

    pub fn max(&mut self, format: &Format) -> &'static Primitive<'static> {
        let primitive = match NumericType::from_format(format) {
            UInt | UFixed => &primitives::numbers::NUM_MAX,
            SInt | SFixed => &primitives::numbers::NUM_SMAX,
        };

        self.import(primitive.import);

        primitive
    }
}

enum NumericType {
    UInt,
    SInt,
    UFixed,
    SFixed,
}

impl NumericType {
    fn from_format(format: &Format) -> NumericType {
        match (format.scale, format.is_signed) {
            (0, false) => NumericType::UInt,
            (0, true) => NumericType::SInt,
            (_, false) => NumericType::UFixed,
            (_, true) => NumericType::SFixed,
        }
    }
}
