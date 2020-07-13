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
