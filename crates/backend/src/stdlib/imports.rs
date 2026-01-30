use std::ops::Index;
use std::{array, iter};

use strum::{EnumCount, VariantArray, VariantNames};
use strum_macros::{EnumCount, VariantArray, VariantNames};

type ImportRepr = u8;

#[derive(Clone, Copy, EnumCount, VariantArray, VariantNames)]
#[repr(u8)]
pub enum Import {
    #[strum(to_string = "primitives/compile.futil")]
    Compile = 1 << 0,
    #[strum(to_string = "primitives/core.futil")]
    Core = 1 << 1,
    #[strum(to_string = "primitives/binary_operators.futil")]
    BinaryOperators = 1 << 2,
    #[strum(to_string = "primitives/math.futil")]
    Math = 1 << 3,
    #[strum(to_string = "primitives/numbers.futil")]
    Numbers = 1 << 4,
}

impl Import {
    pub const ALL: &[Import] = <Self as VariantArray>::VARIANTS;
    pub const PATHS: &[&str] = <Self as VariantNames>::VARIANTS;

    #[inline]
    const fn mask(self) -> ImportRepr {
        self as ImportRepr
    }

    #[inline]
    const fn index(self) -> usize {
        (self as ImportRepr).trailing_zeros() as usize
    }
}

#[derive(Clone, Copy)]
pub struct ImportSet(ImportRepr);

impl ImportSet {
    #[inline]
    pub const fn new() -> ImportSet {
        ImportSet(0)
    }

    #[inline]
    pub const fn contains(&self, file: Import) -> bool {
        self.0 & file.mask() != 0
    }

    #[inline]
    pub const fn insert(&mut self, file: Import) {
        self.0 |= file.mask();
    }

    pub fn paths(&self) -> impl Iterator<Item = &'static str> {
        iter::zip(Import::ALL, Import::PATHS).filter_map(|(&import, &path)| {
            self.contains(import).then_some(path)
        })
    }

    pub fn paths_from<'a>(
        &self,
        paths: &'a ImportPaths,
    ) -> impl Iterator<Item = &'a str> {
        iter::zip(Import::ALL, &paths.paths).filter_map(|(&import, path)| {
            self.contains(import).then_some(path.as_str())
        })
    }
}

impl Default for ImportSet {
    #[inline]
    fn default() -> Self {
        ImportSet::new()
    }
}

pub struct ImportPaths {
    paths: [String; Import::COUNT],
}

impl ImportPaths {
    pub fn new<F>(f: F) -> ImportPaths
    where
        F: FnMut(usize) -> String,
    {
        ImportPaths {
            paths: array::from_fn(f),
        }
    }
}

impl Index<Import> for ImportPaths {
    type Output = String;

    #[inline]
    fn index(&self, file: Import) -> &Self::Output {
        &self.paths[file.index()]
    }
}
