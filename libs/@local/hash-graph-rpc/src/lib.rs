#![feature(impl_trait_in_assoc_type)]
#![feature(marker_trait_attr)]
#![feature(never_type)]
#![feature(associated_type_bounds)]
#![feature(type_alias_impl_trait)]
extern crate core;

pub mod client;
pub mod rpc;
pub mod server;
pub mod specification;
mod types;
