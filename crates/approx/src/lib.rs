//! Function approximations.

pub mod addressing;
pub mod datapath;
pub mod faithful;
pub mod remez;

pub use addressing::{AddressSpec, TableDomain};
pub use datapath::{Datapath, HornerRanges};
pub use faithful::PolynomialApprox;
