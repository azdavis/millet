mod event;
mod grammar;
mod parser;
mod sink;
mod source;

use crate::parser::{ParseError, Parser};
use lexer::Lexer;
use rowan::GreenNode;
use sink::Sink;
use source::Source;
use syntax::SyntaxNode;

pub fn parse(input: &str) -> Parse {
  let tokens: Vec<_> = Lexer::new(input).collect();
  let source = Source::new(&tokens);
  let parser = Parser::new(source);
  let events = parser.parse();
  let sink = Sink::new(&tokens, events);

  sink.finish()
}

pub struct Parse {
  green_node: GreenNode,
  errors: Vec<ParseError>,
}

impl Parse {
  pub fn debug_tree(&self) -> String {
    let mut s = String::new();

    let syntax_node = SyntaxNode::new_root(self.green_node.clone());
    let tree = format!("{:#?}", syntax_node);

    // Remove last byte because formatting `SyntaxNode` adds a newline to the end
    s.push_str(&tree[0..tree.len() - 1]);

    for error in &self.errors {
      s.push_str(&format!("\n{}", error));
    }

    s
  }
}

#[cfg(test)]
pub(crate) fn check(input: &str, expected_tree: expect_test::Expect) {
  expected_tree.assert_eq(&parse(input).debug_tree())
}
