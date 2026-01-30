//! Arbitrary-precision constants.

use calyx_ir as ir;
use malachite::Natural;

use calyx_libm_utils::Diagnostic;
use calyx_libm_utils::mangling::mangle;

use super::PrimitiveBuilder;

pub struct Constant<'a> {
    pub width: u64,
    pub value: &'a Natural,
}

impl Constant<'_> {
    fn body(&self) -> String {
        format!("assign out = {}'h{:x};", self.width, self.value)
    }
}

impl PrimitiveBuilder for Constant<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!("const", self.width, self.value))
    }

    fn build(&self, name: ir::Id) -> Result<ir::Primitive, Diagnostic> {
        let mut share = ir::Attributes::default();
        share.insert(ir::Attribute::Bool(ir::BoolAttr::Share), 1);

        Ok(ir::Primitive {
            name,
            params: vec![],
            signature: vec![ir::PortDef::new(
                "out",
                ir::Width::Const { value: self.width },
                ir::Direction::Output,
                Default::default(),
            )],
            attributes: share,
            is_comb: true,
            latency: None,
            body: Some(self.body()),
        })
    }
}
