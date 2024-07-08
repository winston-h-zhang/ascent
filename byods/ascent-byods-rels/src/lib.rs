//! data structures for [`ascent`](https://github.com/s-arash/ascent) relations,
//! made possible by Ascent's [BYODS](https://dl.acm.org/doi/pdf/10.1145/3622840) feature

#![cfg_attr(not(test), deny(unused_crate_dependencies))]

// See Cargo.toml for why this is needed.
use syn as _;

#[doc(hidden)]
pub mod eqrel_binary;
#[doc(hidden)]
pub mod eqrel_ind;
#[doc(hidden)]
pub mod eqrel_ternary;
#[doc(hidden)]
pub mod fake_vec;
mod iterator_from_dyn;
mod rel_boilerplate;
mod test;
#[doc(hidden)]
pub mod trrel_binary;
#[doc(hidden)]
pub mod trrel_ternary_ind;
mod union_find;
mod utils;

#[doc(hidden)]
pub mod binary_rel;
#[doc(hidden)]
pub mod ceqrel_ind;
#[doc(hidden)]
pub mod trrel_binary_ind;
pub mod trrel_union_find;
#[doc(hidden)]
pub mod trrel_union_find_binary_ind;

pub mod adaptor;
pub mod eqrel;
pub mod trrel;
pub mod trrel_uf;
pub mod uf;
