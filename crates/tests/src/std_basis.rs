use crate::check::{check_with_std_basis, fail_with_std_basis};

#[test]
fn smoke() {
  check_with_std_basis("");
}

#[test]
fn option() {
  check_with_std_basis(
    r#"
val _ = Option.valOf (SOME 3) : int
val _ = Option.getOpt (SOME 3, 123) : int
val _ = Option.getOpt (NONE, false) : bool
val _ = Option.map (fn x => x + 5) (SOME 5) : int option
val _ = Option.map (fn x => x + 5) NONE : int option
val _ = Option.join (SOME (SOME "hey")) : string option
"#,
  );
}

#[test]
fn list() {
  check_with_std_basis(
    r#"
val _ = List.length [1, 2] : int
val _ = List.null [] : bool
val _ = List.map (fn x => x = 3) [4, 3, 6] : bool list
val _: int list = [1, 2] @ [3, 4]
"#,
  );
}

#[test]
fn list_pair() {
  check_with_std_basis(
    r#"
val _ = ListPair.zip ([1, 4], ["hi", "bye"]) : (int * string) list
"#,
  );
}

#[test]
fn int() {
  check_with_std_basis(
    r#"
val _ = Int.toString 3 : string
val _ =
  case Int.compare (1, 4) of
    LESS => "small"
  | EQUAL => "same"
  | GREATER => "big"

val x = 3 : Int.int
val _ = Int.toInt x
val _ = Int.fromInt x
val _ = LargeInt.fromInt x

val y = x : int
val _ = Int.toInt y
val _ = Int.fromInt y
val _ = LargeInt.fromInt y

val a = Int.toLarge 123
val b = LargeInt.toLarge a
val _ = Int.fromLarge a
val _ = LargeInt.fromLarge a
val _ = Int.fromLarge b
val _ = LargeInt.fromLarge b
"#,
  );
}

#[test]
fn general() {
  check_with_std_basis(
    r#"
val _ = 0 handle
    Bind => 1
  | Match => 2
  | Chr => 3
  | Div => 4
  | Domain => 5
  | Fail s => 6
  | Overflow => 7
  | Size => 8
  | Span => 9
  | Subscript => 10

val _ = exnName Bind
val _ = exnMessage Match

val x = ref 3 : int ref
val _ = !x : int
val () = x := 4

fun add x y = x + y
fun mul x y = x * y
val mul2AfterAdd3 = mul 2 o add 3

val () = ignore "hi"
val _: string = "what up" before ()
val _: int = 3 + 4 before ignore "hey"
  "#,
  );
}

#[test]
fn string() {
  check_with_std_basis(
    r#"
val _ = "hello " ^ "world"
"#,
  );
}

#[test]
fn text_io_stream_io_string() {
  fail_with_std_basis(
    r#"
val () = TextIO.output (TextIO.stdErr, "oh no")
"#,
  );
}
