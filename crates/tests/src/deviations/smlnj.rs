//! SML/NJ sometimes [deviates][1] from the Definition. In some of those cases, we deviate in the
//! same/a similar way, and in other cases, we do not deviate.
//!
//! [1]: http://www.mlton.org/SMLNJDeviations

use crate::check::{check, fail};

#[test]
fn op_in_val() {
  check(
    r#"
signature FOO = sig
  val op + : int * int -> int
(**   ^^ specification uses declaration syntax not allowed here *)
end
"#,
  );
}

#[test]
fn op_star() {
  check(
    r#"
val f = (op *)
val _ : int = f (2, 3)
"#,
  );
}

#[test]
fn rebind_eq() {
  check(
    r#"
val op = = 13
(** ^^^^ cannot re-bind name: = *)
"#,
  );
}

#[test]
fn rebind_true() {
  check(
    r#"
fun true () = ()
(** ^^^^^^^^^^^^ cannot re-bind name: true *)
"#,
  );
}

#[test]
fn rebind_false() {
  check(
    r#"
fun false () = ()
(** ^^^^^^^^^^^^^ cannot re-bind name: false *)
"#,
  );
}

#[test]
fn rebind_nil() {
  check(
    r#"
fun nil () = ()
(** ^^^^^^^^^^^ cannot re-bind name: nil *)
"#,
  );
}

#[test]
fn rebind_cons() {
  check(
    r#"
fun op :: () = ()
(** ^^^^^^^^^^^^^ cannot re-bind name: :: *)
"#,
  );
}

#[test]
fn rebind_ref() {
  check(
    r#"
fun ref () = ()
(** ^^^^^^^^^^^ cannot re-bind name: ref *)
"#,
  );
}

#[test]
fn vector_exp() {
  check(
    r#"
val _ = #[1, 2]
(**     ^^^^^^^ unsupported: vector expressions *)
"#,
  );
}

#[test]
fn vector_pat() {
  check(
    r#"
val _ = fn #[x, 2] => x | _ => 2
(**        ^^^^^^^ unsupported: vector patterns *)
"#,
  );
}

#[test]
fn or_pat() {
  check(
    r#"
datatype foo = Foo of int | Bar of int
val (Foo x | Bar x) = Foo 13
"#,
  );
}

#[test]
fn signature_in_struct_end() {
  check(
    r#"
structure A = struct
    signature B = sig end
(** ^^^^^^^^^^^^^^^^^^^^^ `signature` or `functor` not allowed here *)
end
"#,
  );
}

#[test]
fn functor_in_struct_end() {
  check(
    r#"
structure A = struct
    functor F() = struct end
(** ^^^^^^^^^^^^^^^^^^^^^^^^ `signature` or `functor` not allowed here *)
end
"#,
  );
}

#[test]
fn signature_in_local() {
  check(
    r#"
local
  signature SIG = sig val y : int end
in
  structure S4 : SIG = struct val y = 4 end
  structure S7 : SIG = struct val y = 7 end
end

structure A = S4
structure B = S7
signature C = SIG
(**           ^^^ undefined signature: SIG *)
"#,
  );
}

#[test]
fn functor_in_local() {
  check(
    r#"
local
  functor Func(val x : int) = struct val y = x + 2 end
in
  structure S4 = Func(val x = 4)
  structure S7 = Func(val x = 7)
end

structure A = S4
structure B = S7
structure C = Func(val x = 8)
(**           ^^^^^^^^^^^^^^^ undefined functor: Func *)
"#,
  );
}

#[test]
fn dupe_via_includes() {
  check(
    r#"
signature SIG1 = sig
  type t
  type u
end

signature SIG2 = sig
  type t
  type v
end

signature SIG = sig
  include SIG1
    include SIG2
(** ^^^^^^^^^^^^ duplicate type: t *)
end
"#,
  );
}

#[test]
fn sharing_via_abbreviation_short() {
  check(
    r#"
signature SIG = sig type a = int type b = int sharing type a = b end
(**                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ cannot share type a as int *)
"#,
  );
}

#[test]
fn sharing_via_abbreviation_long() {
  check(
    r#"
signature SIG = sig type a = int * int type b = int * int sharing type a = b end
(**                 ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ cannot share type a as int * int *)
"#,
  );
}

#[test]
fn sharing_via_abbreviation_fst() {
  check(
    r#"
signature S = sig type a = int * int type b sharing type a = b end
(**               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ cannot share type a as int * int *)
"#,
  );
}

#[test]
fn sharing_via_abbreviation_snd() {
  check(
    r#"
signature S = sig type a type b = int * int sharing type a = b end
(**               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ cannot share type b as int * int *)
"#,
  );
}

#[test]
fn multi_where() {
  check(
    r#"
signature S = sig
  type t
  type u = t
end where type u = int
"#,
  );
}

#[test]
fn sharing_and() {
  check(
    r#"
signature S = sig
  type t
  type u
  type v
  sharing type t = u and type u = v
(**                  ^^^ expected `end` *)
end
"#,
  );
}

#[test]
fn with_type_expand() {
  check(
    r#"
type u = real
datatype a = A of t | B of u
withtype u = int and t = u
val _ = A 1.2
"#,
  );
}

#[test]
fn where_structure_1() {
  check(
    r#"
structure S = struct type t = int end
signature SIG = sig structure T : sig type t end end where T = S
"#,
  );
}

#[test]
fn where_structure_2() {
  check(
    r#"
signature FOO = sig type t end
signature BAR = sig structure Foo : FOO end
signature QUZ = sig structure Foo : FOO end
functor F (Bar : BAR) :> QUZ where Foo = Bar.Foo = struct structure Foo = Bar.Foo end
"#,
  );
}

#[test]
fn datatype_copy_non_datatype() {
  check(
    r#"
type ('a, 'b) t = 'a * 'b
datatype u = datatype t
"#,
  );
}

#[test]
fn share_substructure() {
  check(
    r#"
signature SIG = sig
  structure S: sig
    type t
    structure T: sig
      type t
    end
  end
  sharing S = S.T
end
"#,
  );
}

#[test]
fn type_inf_context_1() {
  check(
    r#"
structure S = struct
  val z = (fn x => x) []
(**       ^^^^^^^^^^^^^^ cannot bind expansive polymorphic expression *)
  val y = z :: [true] :: nil
end
"#,
  );
}

#[test]
fn type_inf_context_2() {
  check(
    r#"
structure S : sig val z : bool list end = struct
  val z = (fn x => x) []
(**       ^^^^^^^^^^^^^^ cannot bind expansive polymorphic expression *)
end
"#,
  );
}

#[test]
fn where_structure_poly() {
  check(
    r#"
signature FOO = sig
  type 'a foo
end

signature BAR = sig
  structure Foo : FOO
end

functor Func (Arg : FOO) :> BAR where Foo = Arg = struct
  structure Foo = Arg
end
"#,
  );
}

#[test]
fn where_structure_ty_already_def() {
  fail(
    r#"
signature FOO = sig
  type t
  type foo = t
end

signature BAR = sig
  structure Foo : FOO
end

structure UnitFoo :> FOO = struct
  type t = unit
  type foo = t
end

structure UnitBar : BAR where Foo = UnitFoo = struct
  structure Foo = UnitFoo
end
"#,
  );
}
