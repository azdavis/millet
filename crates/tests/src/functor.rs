use crate::check::check;

#[test]
fn empty() {
  check(
    r#"
functor F (A: sig end) = struct end
structure S = F (struct type t = int end)
"#,
  );
}

#[test]
fn missing_type() {
  check(
    r#"
functor F (A: sig
  type t
end) = struct end
structure S = F (struct end)
(**              ^^^^^^^^^^ undefined type: t *)
"#,
  );
}

#[test]
fn wrong_id_status() {
  check(
    r#"
functor F (A: sig
  exception Foo
end) = struct end
structure S = F (struct val Foo = Match end)
(**              ^^^^^^^^^^^^^^^^^^^^^^^^^^ incompatible identifier statuses: Foo *)
"#,
  );
}

#[test]
fn big() {
  check(
    r#"
signature SIG = sig
  type t
  val foo: t
  val bar: int -> t
  val quz: t -> int
  val cmb: t -> t -> t
end

functor F (A: SIG) = struct
  type heh = A.t list
  val what: A.t = A.cmb A.foo A.foo
  fun hm (xs: heh): A.t =
    case xs of
      [] => A.foo
    | x :: xs => A.cmb (A.bar (A.quz x + 3)) (hm xs)
end

structure Impl = struct
  type t = string
  val foo = "heh"
  fun bar _ = "no"
  fun quz _ = 3
  fun cmb _ y = y
end

structure S = F (Impl)

val xs: S.heh = ["yes", Impl.foo, S.what]
val guh: Impl.t = S.hm xs
val done: string list = [guh, Impl.foo, S.what, "why"]
"#,
  );
}

#[test]
fn id_uses_param_sig() {
  check(
    r#"
functor Id (S: sig end) = S

structure Guy = Id (struct
  val x = 3
end)

val _: int = Guy.x
(**          ^^^^^ undefined value: x *)
"#,
  );
}

#[test]
fn output_uses_output_sig() {
  check(
    r#"
signature SIG = sig
  val x: int
end

functor F (A: SIG): sig end = A

structure S = F (struct
  val x = 3
end)

val _ = S.x
(**     ^^^ undefined value: x *)
"#,
  );
}

#[test]
fn datatype_generate() {
  check(
    r#"
functor F (A: sig end) = struct
  datatype t = C
  fun f C = ()
end

structure S = struct end

structure One = F (S)
structure Two = F (S)

val _ = One.f Two.C
(**           ^^^^^ expected t, found t *)
"#,
  );
}

#[test]
fn transparent_id() {
  check(
    r#"
signature SIG = sig
  type t
end

functor Id (S: SIG) = S

structure A = struct
  type t = int
end

structure B = Id (A)

val _: A.t = 3
val _: B.t = 3
"#,
  );
}

#[test]
fn transparent_id_big() {
  check(
    r#"
signature SIG = sig
  type t
  val x: t
  val f: t -> t
end

functor Id (S: SIG) = S

structure A = struct
  type t = int
  val x = 3
  fun f y = y
end

structure B = Id (A)
structure C = Id (A)
structure D = Id (B)

val _ = B.f (A.f A.x)
val _ = C.f (B.f A.x)
val _ = D.f (C.f A.x)
val _ = A.f (D.f A.x)

val _ = B.f (A.f B.x)
val _ = C.f (B.f B.x)
val _ = D.f (C.f B.x)
val _ = A.f (D.f B.x)

val _ = B.f (A.f C.x)
val _ = C.f (B.f C.x)
val _ = D.f (C.f C.x)
val _ = A.f (D.f C.x)

val _ = B.f (A.f D.x)
val _ = C.f (B.f D.x)
val _ = D.f (C.f D.x)
val _ = A.f (D.f D.x)
"#,
  );
}

#[test]
fn open_datatype_ctors_not_in_sig() {
  check(
    r#"
structure A = struct
  datatype t = T
end

signature SIG = sig
  type t
end

functor F (Arg: SIG) = struct
  open Arg
end

structure R = F (A)

val _ = A.T
val _ = R.T
(**     ^^^ undefined value: T *)
"#,
  );
}

#[test]
fn open_datatype_ctors_in_sig() {
  check(
    r#"
structure A = struct
  datatype t = B | C
end

signature SIG = sig
  datatype t = B | C
end

functor F (Arg: SIG) = struct
  open Arg
end

structure R = F (A)

val _ =
  case A.B of
    R.B => 1
  | R.C => 2
"#,
  );
}

#[test]
fn arg_shorthand() {
  check(
    r#"
functor F (A: sig end) = struct end
structure A = F ()
structure B = F (val x = 3)
structure C = F (type t = int)
structure D = F (val a = 1 val b = 2)
"#,
  );
}

#[test]
fn param_shorthand() {
  check(
    r#"
functor Empty () = struct end
functor F (type t val x: t) = struct end
structure A = F (type t = int val x = 3)
structure B = F (type t = string val x = "hi")
structure C = F (type t = unit val x = ())
structure D = F (type t = bool)
(**           ^^^^^^^^^^^^^^^^^ missing value required by signature: x *)
"#,
  );
}
