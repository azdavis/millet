mod lex;
mod loc;
mod source_file;
mod token;

pub fn get(file: &source_file::SourceFile) -> usize {
  let ts = lex::get(file);
  123
}
