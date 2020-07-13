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
