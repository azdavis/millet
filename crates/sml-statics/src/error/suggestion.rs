//! See [`get`].

/// Suggests a bit of syntax that may be similar to the input.
pub(crate) fn get(s: &str) -> Option<&'static str> {
  let ret = match s {
    "func" | "function" | "def" => "fun",
    "lambda" => "fn",
    "const" | "var" => "val",
    "enum" => "datatype",
    "begin" => "local",
    "module" | "namespace" => "structure",
    "match" | "switch" => "case",
    "elsif" | "elif" => "else if",
    "integer" => "int",
    "Integer" => "Int",
    "boolean" => "bool",
    "Boolean" => "Bool",
    "character" | "rune" => "char",
    "Character" | "Rune" => "Char",
    "uint" | "unsigned" => "word",
    "Uint" | "UInt" | "Unsigned" => "Word",
    "void" => "unit",
    "float" | "double" => "real",
    "Float" | "Double" => "Real",
    "str" => "string",
    "Str" => "String",
    "vec" => "vector",
    "Vec" => "Vector",
    "Optional" | "Maybe" => "option",
    // nil is the empty list
    "none" | "None" | "Nothing" | "null" | "NULL" | "nullptr" | "undefined" => "NONE",
    "some" | "Some" | "Just" => "SOME",
    "True" | "TRUE" | "YES" => "true",
    "False" | "FALSE" | "NO" => "false",
    "==" | "===" => "=",
    "!=" | "!==" => "<>",
    "&&" => "andalso",
    "||" | "or" => "orelse",
    _ => return None,
  };
  Some(ret)
}
