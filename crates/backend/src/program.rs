use std::io;

use calyx_ir as ir;

use crate::{ImportPaths, ImportSet};

pub struct Program {
    pub imports: ImportSet,
    pub context: ir::Context,
}

impl Program {
    pub fn write<W: io::Write>(&self, out: &mut W) -> io::Result<()> {
        for import in self.imports.paths() {
            writeln!(out, "import \"{}\";", import)?;
        }

        ir::Printer::write_context(&self.context, true, out)
    }

    pub fn write_with_paths<W: io::Write>(
        &self,
        paths: &ImportPaths,
        out: &mut W,
    ) -> io::Result<()> {
        for import in self.imports.paths_from(paths) {
            writeln!(out, "import \"{}\";", import)?;
        }

        ir::Printer::write_context(&self.context, true, out)
    }
}
