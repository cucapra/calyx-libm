use std::collections::HashSet;

use calyx_ir as ir;

use calyx_libm_utils::Diagnostic;

use crate::program::Program;
use crate::stdlib::{Import, Importer, build_library};

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
    ) -> Result<ir::Component, Diagnostic>;
}

pub struct ComponentManager {
    pub importer: Importer,
    pub components: Vec<ir::Component>,
    pub library: ir::LibrarySignatures,
    pub ids: Ids,
    generated: HashSet<ir::Id>,
}

impl ComponentManager {
    pub fn new() -> ComponentManager {
        ComponentManager {
            importer: Importer::new(),
            components: Vec::new(),
            library: build_library(),
            ids: Ids::new(),
            generated: HashSet::new(),
        }
    }

    pub fn import(&mut self, file: Import) {
        self.importer.import(file);
    }

    pub fn get_primitive<B: PrimitiveBuilder>(
        &mut self,
        builder: &B,
    ) -> Result<ir::Id, Diagnostic> {
        let name = builder.name();

        if self.generated.insert(name) {
            self.library
                .add_inline_primitive(builder.build(name)?)
                .set_source();
        }

        Ok(name)
    }

    pub fn get<B: ComponentBuilder>(
        &mut self,
        builder: &B,
    ) -> Result<(ir::Id, Vec<ir::PortDef<u64>>), Diagnostic> {
        let name = builder.name();

        if self.generated.insert(name) {
            let component = builder.build(name, self)?;

            self.components.push(component);
        }

        Ok((name, builder.signature()))
    }

    pub fn into_program(self) -> Program {
        Program {
            imports: self.importer.into_imports(),
            context: ir::Context {
                components: self.components,
                lib: self.library,
                entrypoint: Default::default(),
                bc: Default::default(),
                extra_opts: Default::default(),
                metadata: None,
            },
        }
    }
}

pub struct Ids {
    pub in_: ir::Id,
    pub out: ir::Id,
    pub left: ir::Id,
    pub right: ir::Id,
}

impl Ids {
    fn new() -> Ids {
        Ids {
            in_: ir::Id::new("in"),
            out: ir::Id::new("out"),
            left: ir::Id::new("left"),
            right: ir::Id::new("right"),
        }
    }
}
