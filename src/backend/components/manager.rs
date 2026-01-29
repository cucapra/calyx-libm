use std::collections::HashSet;

use calyx_ir as ir;

use calyx_libm_utils::Diagnostic;

use crate::backend::stdlib::{Import, Importer};

pub trait PrimitiveBuilder {
    fn name(&self) -> ir::Id;

    fn build(&self, name: ir::Id) -> Result<ir::Primitive, Diagnostic>;
}

pub trait ComponentBuilder {
    fn name(&self) -> ir::Id;

    fn signature(&self) -> Vec<ir::PortDef<u64>>;

    fn build(
        &self,
        name: ir::Id,
        cm: &mut ComponentManager,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<ir::Component, Diagnostic>;
}

pub struct ComponentManager {
    pub importer: Importer,
    pub components: Vec<ir::Component>,
    generated: HashSet<ir::Id>,
}

impl ComponentManager {
    pub fn new() -> ComponentManager {
        ComponentManager {
            importer: Importer::new(),
            components: Vec::new(),
            generated: HashSet::new(),
        }
    }

    pub fn import(&mut self, file: Import) {
        self.importer.import(file);
    }

    pub fn get_primitive<B: PrimitiveBuilder>(
        &mut self,
        builder: &B,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<ir::Id, Diagnostic> {
        let name = builder.name();

        if self.generated.insert(name) {
            lib.add_inline_primitive(builder.build(name)?).set_source();
        }

        Ok(name)
    }

    pub fn get<B: ComponentBuilder>(
        &mut self,
        builder: &B,
        lib: &mut ir::LibrarySignatures,
    ) -> Result<(ir::Id, Vec<ir::PortDef<u64>>), Diagnostic> {
        let name = builder.name();

        if self.generated.insert(name) {
            let component = builder.build(name, self, lib)?;

            self.components.push(component);
        }

        Ok((name, builder.signature()))
    }
}
