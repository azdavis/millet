//! Some Rust-y types and functions ported (back?) to SML.

use crate::check::check;

#[test]
fn option() {
  check(
    r#"
(* Rust's Option<T> *)

datatype 't option = None | Some of 't
exception Unwrap

fun ('t, 'u) map (self: 't option) (f: 't -> 'u): 'u option =
  case self of
    None => None
  | Some x => Some (f x)

fun 't unwrap (self: 't option): 't =
  case self of
    None => raise Unwrap
  | Some x => x

fun 't unwrap_or (self: 't option) (default: 't): 't =
  case self of
    None => default
  | Some x => x

fun 't unwrap_or_else (self: 't option) (f: unit -> 't): 't =
  case self of
    None => f ()
  | Some x => x

fun 't is_some (self: 't option): bool =
  case self of
    None => false
  | Some _ => true

fun 't is_none (self: 't option): bool =
  case self of
    None => true
  | Some _ => false

fun ('t, 'u) and_then (self: 't option) (f: 't -> 'u option): 'u option =
  case self of
    None => None
  | Some x => f x

(* forall x, flatten x = and_then x id *)
fun 't flatten (self: 't option option): 't option =
  case self of
    None => None
  | Some None => None
  | Some (Some x) => Some x
"#,
  );
}

#[test]
fn result() {
  check(
    r#"
(* Rust's Result<T, E> *)

datatype ('t, 'e) result = Ok of 't | Err of 'e
exception Unwrap

fun ('t, 'e, 'u) map (self: ('t, 'e) result) (f: 't -> 'u): ('u, 'e) result =
  case self of
    Ok x => Ok (f x)
  | Err e => Err e

fun ('t, 'e) unwrap (self: ('t, 'e) result): 't =
  case self of
    Ok x => x
  | Err _ => raise Unwrap

fun ('t, 'e) unwrap_or (self: ('t, 'e) result) (default: 't): 't =
  case self of
    Ok x => x
  | Err _ => default

fun ('t, 'e) unwrap_or_else (self: ('t, 'e) result) (f: unit -> 't): 't =
  case self of
    Ok x => x
  | Err _ => f ()

fun ('t, 'e) is_ok (self: ('t, 'e) result): bool =
  case self of
    Ok _ => true
  | Err _ => false

fun ('t, 'e) is_err (self: ('t, 'e) result): bool =
  case self of
    Ok _ => false
  | Err _ => true

fun ('t, 'e, 'u) and_then (self: ('t, 'e) result) (f: 't -> ('u, 'e) result): ('u, 'e) result =
  case self of
    Ok x => f x
  | Err e => Err e

(* forall x, flatten x = and_then x id *)
fun ('t, 'e) flatten (self: (('t, 'e) result, 'e) result): ('t, 'e) result =
  case self of
    Ok (Ok x) => Ok x
  | Ok (Err e) => Err e
  | Err e => Err e
"#,
  );
}
