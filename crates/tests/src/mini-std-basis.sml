(* doesn't need to be right, just needs to type-check! *)

exception Overflow

structure Int = struct
  fun max (_ : int, _ : int) = 0
  fun toString (_ : int) = ""
end

structure Real = struct
  fun abs (_ : real) = 0.0
end

structure List = struct
  exception Empty

  fun map f xs =
    case xs of
      nil => nil
    | x :: xs => f x :: map f xs

  fun foldl f z xs =
    case xs of
      nil => nil
    | x :: xs => foldl f (f (x, z)) xs

  fun null xs =
    case xs of
      nil => true
    | _ :: _ => false
end

fun hd xs =
  case xs of
    nil => raise List.Empty
  | x :: _ => x

fun tl xs =
  case xs of
    nil => raise List.Empty
  | _ :: xs => xs

fun (xs @ ys) =
  case xs of
    nil => ys
  | x :: xs => x :: xs @ ys

datatype 'a option =
  NONE
| SOME of 'a

structure Option = struct
  exception Option

  fun isSome x =
    case x of
      NONE => false
    | SOME _ => true
end

fun valOf x =
  case x of
    NONE => raise Option.Option
  | SOME x => x

fun ((_ : 'a ref) := (_ : 'a)) = ()

fun ((_ : string) ^ (_ : string)) = ""

fun ! (ref v) = v

fun print (_ : string) = ()
