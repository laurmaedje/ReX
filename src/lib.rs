#[macro_use]
extern crate serde_derive;

#[macro_use]
extern crate log;

#[macro_use]
extern crate static_map;
#[macro_use]
extern crate static_map_macros;

#[macro_use]
mod macros;

pub mod environments;
pub mod error;
pub mod dimensions;
pub mod layout;
pub mod lexer;
pub mod parser;
pub mod render;

pub mod font;
mod functions;

pub use render::*;
