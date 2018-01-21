/// Core OpenMaya data structures.
#[macro_use] mod core;
pub use OpenMaya::core::*;

/// Maya proxy objects, implemented via Rust traits.
mod mpx;
pub use OpenMaya::mpx::*;

// Maya function sets, implemented as Rust wrapper structs.
mod mfn;
pub use OpenMaya::mfn::*;