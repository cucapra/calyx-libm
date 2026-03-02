use std::path::PathBuf;

use calyx_frontend::{LibrarySignatures, NamespaceDef};
use calyx_stdlib::{COMPILE_LIB, KNOWN_LIBS};

const NUMBERS_LIB: (&str, &str) = (
    "numbers.futil",
    include_str!("../../../../primitives/numbers.futil"),
);

pub fn build_library() -> LibrarySignatures {
    let (_, compile) = COMPILE_LIB;
    let (_, numbers) = NUMBERS_LIB;

    let [
        (_, [(_, core), _]),
        (_, [(_, binary_operators), _]),
        (_, [(_, math), _]),
        ..,
    ] = KNOWN_LIBS;

    let mut lib = LibrarySignatures::default();

    for source in [compile, core, binary_operators, math, numbers] {
        let ns = NamespaceDef::construct_from_str(source).unwrap();

        for (path, primitives) in ns.externs {
            if path.is_some() {
                for primitive in primitives {
                    lib.add_extern_primitive(PathBuf::new(), primitive);
                }
            } else {
                for primitive in primitives {
                    lib.add_inline_primitive(primitive);
                }
            }
        }
    }

    lib
}
