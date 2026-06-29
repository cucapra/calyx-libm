use calyx_ir as ir;

use calyx_libm_hir as hir;
use calyx_libm_utils::Diagnostic;

use crate::ComponentManager;

pub use crate::stdlib::Primitive;

pub struct Operator<'hir> {
    pub idx: hir::SollyaIdx,
    pub span: hir::Span,
    pub ctx: &'hir hir::Context,
}

impl Operator<'_> {
    pub fn sollya(&self) -> impl std::fmt::Display {
        self.idx.sollya(self.ctx)
    }

    pub fn pretty(&self) -> impl std::fmt::Display {
        self.idx.pretty(self.ctx)
    }

    pub fn prefix_hint(&self) -> &'static str {
        self.idx.name(self.ctx).unwrap_or("f")
    }
}

pub struct Component {
    pub name: ir::Id,
    pub prefix_hint: ir::Id,
    pub signature: Vec<ir::PortDef<u64>>,
    pub is_comb: bool,
}

pub enum Prototype {
    Prim(&'static Primitive<'static>),
    Comp(Component),
}

pub trait OpBuilder {
    fn build(
        &self,
        op: &Operator,
        cm: &mut ComponentManager,
    ) -> Result<Prototype, Diagnostic>;
}
