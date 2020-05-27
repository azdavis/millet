fn run() -> bool {
  println!("Hello, world!");
  true
}

fn main() {
  if !run() {
    std::process::exit(1);
  }
}
