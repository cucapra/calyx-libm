use std::fmt::Write;

use calyx_ir as ir;
use malachite::Natural;

use calyx_libm_utils::Diagnostic;
use calyx_libm_utils::mangling::{Hash, mangle};

use super::PrimitiveBuilder;

pub struct Rom<'a> {
    pub idx_width: u64,
    pub out_width: u64,
    pub data: &'a [Natural],
}

impl Rom<'_> {
    fn body(&self) -> String {
        let mut body = String::from(concat!(
            "always_comb begin\n",
            "    unique case (idx)\n",
        ));

        for (i, val) in self.data.iter().enumerate() {
            writeln!(
                body,
                "      {}'d{}: out = {}'h{:x};",
                self.idx_width, i, self.out_width, val,
            )
            .unwrap();
        }

        body.push_str(concat!(
            "      default out = 'x;\n",
            "    endcase\n",
            "  end",
        ));

        body
    }
}

impl PrimitiveBuilder for Rom<'_> {
    fn name(&self) -> ir::Id {
        ir::Id::new(mangle!(
            "rom",
            self.idx_width,
            self.out_width,
            Hash::new(self.data),
        ))
    }

    fn build(&self, name: ir::Id) -> Result<ir::Primitive, Diagnostic> {
        let mut data = ir::Attributes::default();
        let mut share = ir::Attributes::default();

        data.insert(ir::Attribute::Bool(ir::BoolAttr::Data), 1);
        share.insert(ir::Attribute::Bool(ir::BoolAttr::Share), 1);

        Ok(ir::Primitive {
            name,
            params: vec![],
            signature: vec![
                ir::PortDef::new(
                    "idx",
                    ir::Width::Const {
                        value: self.idx_width,
                    },
                    ir::Direction::Input,
                    data,
                ),
                ir::PortDef::new(
                    "out",
                    ir::Width::Const {
                        value: self.out_width,
                    },
                    ir::Direction::Output,
                    Default::default(),
                ),
            ],
            attributes: share,
            is_comb: true,
            latency: None,
            body: Some(self.body()),
        })
    }
}
