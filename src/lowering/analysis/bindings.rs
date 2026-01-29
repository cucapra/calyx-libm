use std::collections::HashMap;

use super::passes::{Pass, PassManager};
use crate::fpcore::{Visitor, ast, visitor};
use crate::utils::{Diagnostic, Reporter};

#[derive(Clone, Copy)]
pub enum Binding<'ast> {
    Argument(&'ast ast::Argument),
    Let(&'ast ast::Binding),
    Mut(&'ast ast::MutableVar),
    Index(&'ast ast::InductionVar),
}

pub struct NameResolution<'ast> {
    pub defs: HashMap<ast::Id, &'ast ast::FPCore>,
    pub names: HashMap<ast::NodeId, Binding<'ast>>,
}

impl<'ast> Pass<'ast> for NameResolution<'ast> {
    fn run(pm: &PassManager<'_, 'ast, '_>) -> Option<Self> {
        let defs = gather_definitions(pm)?;

        let mut builder = Builder {
            reporter: &mut pm.rpt(),
            result: NameResolution {
                defs,
                names: HashMap::new(),
            },
            scopes: Vec::new(),
        };

        builder.visit_definitions(pm.ast()).ok()?;

        Some(builder.result)
    }
}

fn gather_definitions<'ast>(
    pm: &PassManager<'_, 'ast, '_>,
) -> Option<HashMap<ast::Id, &'ast ast::FPCore>> {
    let mut defs = HashMap::new();

    for def in pm.ast() {
        if let Some(sym) = &def.name {
            let prev = defs.insert(sym.id, def);

            if let Some(prev) = prev.and_then(|prev| prev.name.as_ref()) {
                pm.rpt().emit(
                    &Diagnostic::error()
                        .with_message(format!(
                            "redefinition of operator `{}`",
                            prev.id,
                        ))
                        .with_secondary(prev.span, "name first defined here")
                        .with_primary(sym.span, "name already defined"),
                );

                return None;
            }
        }
    }

    Some(defs)
}

#[derive(Debug)]
struct ResolutionError;

struct Builder<'pm, 'ast, 'src> {
    reporter: &'pm mut Reporter<'src>,
    result: NameResolution<'ast>,
    scopes: Vec<HashMap<ast::Id, Binding<'ast>>>,
}

impl<'ast> Builder<'_, 'ast, '_> {
    fn find_name(&self, symbol: ast::Id) -> Option<&Binding<'ast>> {
        for map in self.scopes.iter().rev() {
            if let Some(name) = map.get(&symbol) {
                return Some(name);
            }
        }

        None
    }

    fn visit_properties(
        &mut self,
        props: &'ast [ast::Property],
    ) -> Result<(), ResolutionError> {
        for prop in props {
            if let ast::PropKind::Pre(expr) = &prop.kind {
                self.visit_expression(expr)?;
            } else {
                self.warn_ignored(prop);
            }
        }

        Ok(())
    }

    fn warn_ignored(&mut self, prop: &ast::Property) {
        if !is_silent(&prop.kind) {
            self.reporter.emit(
                &Diagnostic::warning()
                    .with_message(format!(
                        "ignoring unsupported property `:{}`",
                        prop.kind.name(),
                    ))
                    .with_primary(prop.span, "unsupported property"),
            );
        }
    }
}

impl<'ast> Visitor<'ast> for Builder<'_, 'ast, '_> {
    type Error = ResolutionError;

    fn visit_definition(
        &mut self,
        def: &'ast ast::FPCore,
    ) -> Result<(), ResolutionError> {
        self.scopes.clear();

        let mut scope = HashMap::with_capacity(def.args.len());

        for arg in &def.args {
            if !arg.dims.is_empty() {
                self.reporter.emit(
                    &Diagnostic::error()
                        .with_message("tensor arguments not supported")
                        .with_primary(
                            arg.var.span,
                            "unsupported argument type",
                        ),
                );

                return Err(ResolutionError);
            }

            scope.insert(arg.var.id, Binding::Argument(arg));

            self.visit_properties(&arg.props)?;
        }

        self.scopes.push(scope);

        self.visit_properties(&def.props)?;
        self.visit_expression(&def.body)
    }

    fn visit_expression(
        &mut self,
        expr: &'ast ast::Expression,
    ) -> Result<(), ResolutionError> {
        match &expr.kind {
            ast::ExprKind::Num(_) => Ok(()),
            ast::ExprKind::Const(_) => Ok(()),
            ast::ExprKind::Id(sym) => {
                if let Some(binding) = self.find_name(sym.id) {
                    self.result.names.insert(expr.uid, *binding);

                    Ok(())
                } else {
                    self.reporter.emit(
                        &Diagnostic::error()
                            .with_message(format!(
                                "undefined name `{}`",
                                sym.id,
                            ))
                            .with_primary(sym.span, "undefined name"),
                    );

                    Err(ResolutionError)
                }
            }
            ast::ExprKind::Op(op, _) => {
                if let ast::OpKind::FPCore(id) = op.kind
                    && !self.result.defs.contains_key(&id)
                {
                    self.reporter.emit(
                        &Diagnostic::error()
                            .with_message(format!("undefined operator `{id}`"))
                            .with_primary(op.span, "undefined operator"),
                    );

                    return Err(ResolutionError);
                }

                visitor::visit_expression(self, expr)
            }
            ast::ExprKind::If { .. } => visitor::visit_expression(self, expr),
            ast::ExprKind::Let {
                bindings,
                body,
                sequential: false,
            } => {
                let mut scope = HashMap::with_capacity(bindings.len());

                for binding in bindings {
                    self.visit_expression(&binding.expr)?;

                    scope.insert(binding.var.id, Binding::Let(binding));
                }

                self.scopes.push(scope);
                self.visit_expression(body)?;
                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::Let {
                bindings,
                body,
                sequential: true,
            } => {
                self.scopes.push(HashMap::new());

                for binding in bindings {
                    self.visit_expression(&binding.expr)?;

                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(binding.var.id, Binding::Let(binding));
                }

                self.visit_expression(body)?;
                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::While {
                cond,
                vars,
                body,
                sequential: false,
            } => {
                let mut scope = HashMap::with_capacity(vars.len());

                for var in vars {
                    self.visit_expression(&var.init)?;

                    scope.insert(var.var.id, Binding::Mut(var));
                }

                self.scopes.push(scope);
                self.visit_expression(cond)?;
                self.visit_expression(body)?;

                for var in vars {
                    self.visit_expression(&var.update)?;
                }

                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::While {
                cond,
                vars,
                body,
                sequential: true,
            } => {
                self.scopes.push(HashMap::new());

                for var in vars {
                    self.visit_expression(&var.init)?;

                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(var.var.id, Binding::Mut(var));
                }

                self.visit_expression(cond)?;
                self.visit_expression(body)?;

                for var in vars {
                    self.visit_expression(&var.update)?;

                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(var.var.id, Binding::Mut(var));
                }

                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::For {
                indices,
                vars,
                body,
                sequential: false,
            } => {
                let mut scope =
                    HashMap::with_capacity(indices.len() + vars.len());

                for var in indices {
                    self.visit_expression(&var.size)?;

                    scope.insert(var.var.id, Binding::Index(var));
                }

                for var in vars {
                    self.visit_expression(&var.init)?;

                    scope.insert(var.var.id, Binding::Mut(var));
                }

                self.scopes.push(scope);

                for var in vars {
                    self.visit_expression(&var.update)?;
                }

                self.visit_expression(body)?;
                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::Tensor { indices, body } => {
                let mut scope = HashMap::with_capacity(indices.len());

                for var in indices {
                    self.visit_expression(&var.size)?;

                    scope.insert(var.var.id, Binding::Index(var));
                }

                self.scopes.push(scope);
                self.visit_expression(body)?;
                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::For {
                indices,
                vars,
                body,
                sequential: true,
            }
            | ast::ExprKind::TensorStar {
                indices,
                vars,
                body,
            } => {
                for var in indices {
                    self.visit_expression(&var.size)?;
                }

                let bind_indices = |scope: &mut HashMap<_, _>| {
                    for var in indices {
                        scope.insert(var.var.id, Binding::Index(var));
                    }
                };

                self.scopes.push(HashMap::new());

                for var in vars {
                    self.visit_expression(&var.init)?;

                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(var.var.id, Binding::Mut(var));
                }

                bind_indices(self.scopes.last_mut().unwrap());

                for var in vars {
                    self.visit_expression(&var.update)?;

                    self.scopes
                        .last_mut()
                        .unwrap()
                        .insert(var.var.id, Binding::Mut(var));
                }

                self.visit_expression(body)?;
                self.scopes.pop();

                Ok(())
            }
            ast::ExprKind::Cast(_) => visitor::visit_expression(self, expr),
            ast::ExprKind::Array(_) => visitor::visit_expression(self, expr),
            ast::ExprKind::Annotation { props, body } => {
                self.visit_properties(props)?;
                self.visit_expression(body)
            }
        }
    }
}

/// Whether the property can be safely ignored without emitting a warning.
fn is_silent(prop: &ast::PropKind) -> bool {
    matches!(
        prop,
        ast::PropKind::Name(_)
            | ast::PropKind::Description(_)
            | ast::PropKind::Cite(_)
            | ast::PropKind::Pre(_)
            | ast::PropKind::Spec(_)
            | ast::PropKind::Alt(_)
            | ast::PropKind::Example(_)
            | ast::PropKind::CalyxDomain(_)
            | ast::PropKind::CalyxImpl(_)
    )
}
