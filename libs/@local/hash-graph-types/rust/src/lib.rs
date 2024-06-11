#![feature(lint_reasons)]
#![feature(gen_blocks)]

extern crate alloc;

pub mod knowledge;
pub mod ontology;

pub mod owned_by_id;

pub mod account;

pub use self::embedding::Embedding;

mod embedding;
