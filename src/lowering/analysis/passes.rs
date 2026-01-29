use std::cell::{OnceCell, RefCell, RefMut};

use crate::fpcore::ast;
use crate::opts::Opts;
use crate::utils::Reporter;

pub trait Managed<Cache>
where
    Self: Sized,
{
    fn get(cache: &Cache) -> &OnceCell<Self>;
}

macro_rules! register_passes {
    ($cache:ident<$ast:lifetime> { $( $field:ident: $pass:ty ),* $(,)? }) => {
        #[derive(Default)]
        pub struct $cache<$ast> {
            $( $field: OnceCell<$pass> ),*
        }

        $(
            impl<$ast> Managed<$cache<$ast>> for $pass {
                fn get<'pm>(cache: &'pm $cache<$ast>) -> &'pm OnceCell<Self> {
                    &cache.$field
                }
            }
        )*
    };
}

register_passes!(Cache<'ast> {
    bindings: super::NameResolution<'ast>,
    types: super::TypeCheck,
    call_graph: super::CallGraph<'ast>,
});

pub trait Pass<'ast>
where
    Self: Sized,
{
    fn run(pm: &PassManager<'_, 'ast, '_>) -> Option<Self>;
}

pub struct PassManager<'pm, 'ast, 'src> {
    opts: &'pm Opts,
    defs: &'ast [ast::FPCore],
    rpt: RefCell<&'pm mut Reporter<'src>>,
    cache: Cache<'ast>,
}

impl<'pm, 'ast, 'src> PassManager<'pm, 'ast, 'src> {
    pub fn new(
        opts: &'pm Opts,
        defs: &'ast [ast::FPCore],
        rpt: &'pm mut Reporter<'src>,
    ) -> PassManager<'pm, 'ast, 'src> {
        PassManager {
            opts,
            defs,
            rpt: RefCell::new(rpt),
            cache: Default::default(),
        }
    }

    pub fn opts(&self) -> &'pm Opts {
        self.opts
    }

    pub fn ast(&self) -> &'ast [ast::FPCore] {
        self.defs
    }

    pub fn rpt(&self) -> RefMut<'_, &'pm mut Reporter<'src>> {
        self.rpt.borrow_mut()
    }

    pub fn get_analysis<A>(&self) -> Option<&A>
    where
        A: Pass<'ast> + Managed<Cache<'ast>>,
    {
        let cell = A::get(&self.cache);

        if let Some(val) = cell.get() {
            Some(val)
        } else {
            let _ = cell.set(A::run(self)?);

            Some(cell.get().unwrap())
        }
    }
}
