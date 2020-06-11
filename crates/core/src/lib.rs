//! An implementation of Standard ML.

pub mod ast;
pub mod error;
pub mod ident;
pub mod lex;
pub mod loc;
pub mod parse;
pub mod token;

pub fn get() -> usize {
  123
}
