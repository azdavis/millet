(* doesn't need to be right, just needs to type-check! *)

datatype 'a option =
  NONE
| SOME of 'a

structure Int = struct
  fun max (_ : int, _ : int) = 0
  fun toString (_ : int) = ""
end

structure Real = struct
  fun abs (_ : real) = 0.0
end

structure List = struct
  fun map f xs =
    case xs of
      nil => nil
    | x :: xs => f x :: map f xs

  fun foldl f z xs =
    case xs of
      nil => nil
    | x :: xs => foldl f (f (x, z)) xs
end

fun (xs @ ys) =
  case xs of
    nil => ys
  | x :: xs => x :: xs @ ys

fun ((_ : 'a ref) := (_ : 'a)) = ()

fun ((_ : string) ^ (_ : string)) = ""

fun ! (ref v) = v

fun print (_ : string) = ()
