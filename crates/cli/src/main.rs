mod args;

fn run() -> bool {
  let args = args::get();
  println!("args = {:?}", args);
  true
}

fn main() {
  if !run() {
    std::process::exit(1);
  }
}
