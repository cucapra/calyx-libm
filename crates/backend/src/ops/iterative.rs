use calyx_libm_hir::SollyaFn;
use calyx_libm_utils::{Diagnostic, Format};

use super::{OpBuilder, Operator, Prototype};
use crate::ComponentManager;

pub struct Iterative<'a> {
    pub format: &'a Format,
}

impl OpBuilder for Iterative<'_> {
    fn build(
        &self,
        op: &Operator,
        cm: &mut ComponentManager,
    ) -> Result<Prototype, Diagnostic> {
        if let Some(SollyaFn::Sqrt) = op.idx.function(op.ctx) {
            Ok(Prototype::Prim(cm.importer.sqrt(self.format)))
        } else {
            Err(Diagnostic::error()
                .with_message(format!(
                    "iterative implementation not supported for operator `{}`",
                    op.pretty(),
                ))
                .try_with_primary(op.span, "unsupported implementation"))
        }
    }
}
