use std::collections::hash_map::{Entry, HashMap};

use super::NameResolution;
use super::passes::{Pass, PassManager};
use crate::fpcore::{Visitor, ast, visitor};
use crate::utils::{Diagnostic, Reporter};

pub struct CallGraph<'ast> {
    pub linearized: Vec<&'ast ast::FPCore>,
}

impl<'ast> Pass<'ast> for CallGraph<'ast> {
    fn run(pm: &PassManager<'_, 'ast, '_>) -> Option<Self> {
        let bindings = pm.get_analysis()?;

        let mut builder = Builder {
            bindings,
            reporter: &mut pm.rpt(),
            linearized: Vec::with_capacity(pm.ast().len()),
            stack: Vec::new(),
            color: HashMap::with_capacity(bindings.defs.len()),
        };

        builder.visit_definitions(pm.ast()).ok()?;

        Some(CallGraph {
            linearized: builder.linearized,
        })
    }
}

#[derive(Debug)]
struct CycleError;

struct Builder<'pm, 'ast, 'src> {
    bindings: &'pm NameResolution<'ast>,
    reporter: &'pm mut Reporter<'src>,
    linearized: Vec<&'ast ast::FPCore>,
    stack: Vec<ast::Id>,
    color: HashMap<ast::Id, Color>,
}

impl Builder<'_, '_, '_> {
    fn dfs(&mut self) -> Result<(), CycleError> {
        while let Some(&v) = self.stack.last() {
            match self.color.entry(v) {
                Entry::Occupied(mut color) => {
                    self.stack.pop();

                    if *color.get() == Color::Gray {
                        color.insert(Color::Black);

                        self.linearized.push(self.bindings.defs[&v]);
                    }
                }
                Entry::Vacant(color) => {
                    color.insert(Color::Gray);

                    self.visit_definition(self.bindings.defs[&v])?;
                }
            }
        }

        Ok(())
    }
}

impl<'ast> Visitor<'ast> for Builder<'_, 'ast, '_> {
    type Error = CycleError;

    fn visit_definitions<I>(&mut self, defs: I) -> Result<(), CycleError>
    where
        I: IntoIterator<Item = &'ast ast::FPCore>,
    {
        for def in defs {
            if let Some(sym) = &def.name {
                self.stack.push(sym.id);
                self.dfs()?;
            } else {
                self.visit_definition(def)?;
                self.dfs()?;
                self.linearized.push(def);
            }
        }

        Ok(())
    }

    fn visit_expression(
        &mut self,
        expr: &'ast ast::Expression,
    ) -> Result<(), CycleError> {
        if let ast::ExprKind::Op(op, _) = &expr.kind
            && let ast::OpKind::FPCore(id) = op.kind
        {
            match self.color.get(&id) {
                None => {
                    self.stack.push(id);
                }
                Some(Color::Gray) => {
                    self.reporter.emit(
                        &Diagnostic::error()
                            .with_message("cyclic definition")
                            .with_primary(
                                op.span,
                                "cycle introduced by this call",
                            ),
                    );

                    return Err(CycleError);
                }
                Some(Color::Black) => {}
            }
        }

        visitor::visit_expression(self, expr)
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Color {
    Gray,
    Black,
}
