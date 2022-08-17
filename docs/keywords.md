# Keywords

This is documentation for all the keywords in SML, and what they mean.

If an SML keyword is not documented here, please file an issue.

## `exception`

Define an exception.

Exception declarations define new exception constructors.

```sml
exception Foo
exception Bar of string
```

In this example:

- `Foo` has type `exn`
- `Bar` has type `string -> exn`

## `signature`

Define a signature.

Signatures describe the interface to structures.

```sml
signature SIG = sig
  type t
  val x : t
end
```

## `structure`

Define a structure.

Structures are collections of declarations.

```sml
structure Str = struct
  val x = 3
  val y = "hi"
end
```

## `datatype`

Define a new datatype.

Datatype declarations define new types and their constructors.

```sml
datatype debug = On | Off | Level of int
```

In this example:

- `On` and `Off` have type `debug`.
- `Level` has type `int -> debug`.

## `withtype`

Used with `datatype` (and `abstype`) to add extra helper types into scope at the same time as the datatype. This allows the types to be used in the value constructors.

```sml
datatype 'a stream = Nil | Cons of 'a * 'a front
withtype 'a front = unit -> 'a stream
```

## `abstype`

Define an abstract type.

This is not really used in modern SML. Prefer a mix of:

- `datatype`, for creating new types
- `structure`, for managing collections of declarations
- `signature` and ascription, for hiding implementation details

## `andalso`

Compute logical "and" with two `bool` expressions.

```sml
fun canRide height age =
  height > 6 andalso age >= 18
```

It short-circuits, so if the first expression evaluates to `false`, the second is not evaluated.

Because of this special short-circuiting behavior, it is not a regular infix operator, and thus does not work with `op`.

Not to be confused with `and`, which permits declaring multiple things at once.

## `functor`

Define a functor.

Functors are structure-level functions.

```sml
functor F (A : sig
  val x : int
end) = struct
  val y = A.x + 123
end
```

- Regular functions (with `fun` or `fn`) take in values and return values.
- Functors take in structures and return structures.

## `include`

Include a signature in another signature, to merge the two signatures together.

```sml
signature A = sig
  val x : int
end

signature B = sig
  include A
  val y : string
end

structure S : B = struct
  val x = 3
  val y = "hi"
end
```

## `sharing`

Share types in a signature.

```sml
signature A = sig
  type t
  val a : t
end

signature B = sig
  type t
  val b : t
end

signature TXT = sig
  structure A : A
  structure B : B
  sharing type A.t = B.t
end
```

## `eqtype`

Like `type`, but can only be used in signature specifications. It indicates the type must be an equality type.

```sml
signature SIG = sig
  eqtype t
  val x : t
end
```

## `handle`

Handle exceptions.

```sml
val _ = 15 + 150 handle Overflow => 0
```

- If the head expression **does not** raise, the whole expression (i.e. the head expression plus the `handle` clause) evaluates to whatever the head expression did.
- If the head expression **does** raise, and the exception raised **is** matched by the matcher, the whole expression evaluates to the expression in that corresponding matcher arm.
- If the head expression **does** raise, and the exception raised **is not** matched, the whole expression raises.

## `infixr`

Cause the given names to be infix operators with the given precedence (or 0 if none is provided), and right associativity. Similar to `infix`.

```sml
infixr foo
infixr 3 bar quz
```

## `nonfix`

Cause the given names to not be infix.

```sml
nonfix foo
nonfix bar quz
```

## `orelse`

Compute logical "or" with two `bool` expressions.

```sml
fun canAfford price persuasiveness =
  price <= 3 orelse persuasiveness >= 75
```

It short-circuits, so if the first expression evaluates to `true`, the second is not evaluated.

Because of this special short-circuiting behavior, it is not a regular infix operator, and thus does not work with `op`.

## `struct`

Define a structure expression.

Structure expressions are often used as:

- The right-hand side to structure declarations.
- The arguments to functors.

```sml
structure S = struct
  val x = 5
end
```

## `infix`

Cause the given names to be infix operators with the given precedence (or 0 if none is provided), and left associativity. Similar to `infixr`.

```sml
infix foo
infix 3 bar quz
```

## `local`

Limit the scope of declarations.

```sml
local
  val inner = 5
in
  val outerA = inner + 4
  val outerB = inner + 9
end

val _ = outerA + outerB
```

- Declarations in the `local ... in` may be used by declarations in the `in ... end`.
- However, only the declarations in the `in ... end` are in scope outside of the local.

## `raise`

Raise an exception.

```sml
exception Neg

fun fact n =
  if n < 0 then
    raise Neg
  else if n = 0 then
    1
  else
    n * fact (n - 1)
```

A raised exception may be handled with `handle`.

## `where`

Realize a type in a signature.

This allows a type to "break through" opaque ascription and be known to users of a structure that ascribes to a signature.

```sml
signature SIG = sig
  type t
  val x : t
end

structure S :> SIG
  where type t = int =
struct
  type t = int
  val x = 3
end

val _ = S.x : int
```

## `while`

Repeatedly evaluate an expression while a condition is upheld.

```sml
fun sayHi n =
  let
    val r = ref 0
  in
    while !r < n do (
      r := !r + 1;
      print "hi"
    )
  end
```

This feature is "imperative", i.e. it essentially requires the use of `ref` or other side effects to be useful.

## `case`

Attempt to match a "head" expression against a sequence of patterns, and choose the first expression whose pattern matched.

```sml
fun describe n =
  case n of
    0 => "nothing"
  | 1 => "solitary"
  | 2 => "pair"
  | 3 => "triple"
  | _ => "something else"
```

## `else`

Used in conjunction with `if` and `then` to mark the expression that the `if` expression evaluates to if the condition was `false`. Comes after `then`.

```sml
fun choose x =
  if x then "yes" else "no"
```

## `open`

Bring a structure's members into scope.

```sml
structure S = struct
  val x = 5
end

val a = S.x
open S
val b = x
```

## `then`

Used in conjunction with `if` and `else` to mark the expression that the `if` expression evaluates to if the condition was `true`. Comes between `if` and `else`.

```sml
fun choose x =
  if x then "yes" else "no"
```

## `type`

Define a type alias.

```sml
type point = int * int
```

Types defined with `type` are aliases, meaning no "new" types are defined. Rather, the new name is just an alternative name for the right-hand-side type, and aside from name, they are equivalent.

## `with`

Used in abstype declarations, which is to say, not much.

## `and`

Define many things simultaneously.

Most useful for mutually recursive functions.

```sml
fun even 0 = true
  | even n = odd (n - 1)
and odd 0 = false
  | odd n = even (n - 1)
```

Not to be confused with `andalso`, which is for logical "and" of `bool` expressions.

## `end`

Mark the end of various constructs, like:

- `let ... in ... end`
- `local ... in ... end`
- `struct ... end`
- `sig ... end`

## `fun`

Define a function.

```sml
fun inc x = x + 1
```

Functions are the unit of abstraction.

Not to be confused with `fn`, which begins a function (aka lambda) expression.

## `let`

Define a let expression.

```sml
val _ =
  let
    val x = 7
    val y = 5
  in
    x + y
  end
```

- In the `let ... in`, there is a sequence of declarations.
- In the `in ... end`, there is an sequence of expressions (at least 1), separated by `;`.
- The `let` first defines the declarations in the `let ... in`, then evaluates the expressions in the `in ... end`.
- The expressions may use bindings from the declarations.
- The value of the whole `let` is the value of the last expression in the `in ... end`.

## `rec`

Allow a `fn` to be recursive.

```sml
val rec fact = fn
  0 => 1
| n => n * fact (n - 1)
```

Usually, `fun` is preferred.

```sml
fun fact 0 = 1
  | fact n = n * fact (n - 1)
```

## `sig`

Define a signature expression.

Signature expressions are often used as:

- The right-hand side to signature declarations.
- The interfaces to functors.

```sml
signature SIG = sig
  type t
  val x : t
end
```

## `val`

Define value bindings from an expression.

A value declaration evaluates the expression on the right, then matches it with the pattern on the left, and introduces new bindings produced by the pattern.

```sml
val y = 6
val (_, x) = ("ignored", "bound to x")
```

## `as`

Used with patterns to match the entire value to a variable.

```sml
fun f [] = []
  | f (xs as (_ :: xs')) = xs @ f xs'
```

The syntax is `<name> as <pat>`.

## `do`

Separate a `while` loop condition from the loop body.

```sml
val _ = while true do print "y\n"
```

## `fn`

Define a function ("lambda") expression.

```sml
val _ = List.map (fn x => x + 1) [1, 3, 8]
```

Often used to define arguments to higher-order functions, to avoid having to declare and name small helper functions.

## `if`

Case on a `bool`.

```sml
val _ = if 3 > 4 then "math is broken" else "okay good"
```

An if expression cases on a condition of type `bool`, and selects either the `then` expression if the condition was `true`, or the `else` if it was `false`.

## `in`

Separate the first and second parts of a `local` declaration or `let` expression.

```sml
local
  val x = 4
in
  val y = x + 6
end

val _ =
  let
    val z = 5
  in
    z + 8
  end
```

## `of`

1. Separate the head expression of a `case` from the arms.

   ```sml
   fun hm x =
     case x of
       1 => 2
     | _ => 3
   ```

2. Denote that a value or exception constructor takes an argument.

   ```sml
   datatype ans = No | Yes | Custom of string
   ```

## `op`

Cause an infix identifier to temporarily act as though it is not.

```sml
val x : int = op + (1, 2)
```

If the infix identifier is "symbolic" (`+`, `*`, `@`, etc), then no intervening whitespace between the `op` and the identifier is required.

```sml
val xs : int list = op@ ([1, 2], [5, 8])
```

Often used to pass infix operators to higher-order functions.

```sml
val xs : int list = List.map op* [(1, 4), (6, 8), (2, 3)]
```

## `...`

Ignore the rest of the rows in a record pattern.

```sml
val {b, ...} = {a = 1, b = 4}
```

Also used as a expression/type/declaration "hole", though this is not legal SML.

## `->`

Denote a function type.

```sml
type intMaker = unit -> int
```

## `:>`

Opaquely ascribe a structure to a signature.

```sml
structure Stack :> sig
  type 'a t
  val empty : 'a t
  val push : 'a t -> 'a -> 'a t
  val pop : 'a t -> 'a option * 'a t
end = struct
  type 'a t = 'a list
  val empty = []
  fun push xs x = x :: xs
  fun pop xs =
    case xs of
      [] => (NONE, xs)
    | x :: xs' => (SOME x, xs')
end
```

## `=>`

Separate a pattern from its expression in a matcher arm.

```sml
val describe = fn
  0 => "nothing"
| 1 => "single"
| 2 => "dual"
| _ => "other"
```

## `#`

Select a field from a record.

Tuples are record whose labels are numbers starting at 1, so these work with tuples too.

```sml
val x : int = #2 (false, 5)
```

## `(`

Group things together. Can be used to override precedence.

```sml
fun doMath a b c = a * (b + c)
```

## `)`

The companion of `(`.

## `*`

1. Separate types in a tuple type.

   ```sml
   type pair = int * string
   ```

2. Multiply numbers.

   ```sml
   val _ = 3 * 6
   ```

## `,`

Separate things, like tuple elements, list elements, and type arguments.

```sml
val _ = (false, 5)
val _ = [1, 4, 3]
type ('a, 'b) parser = 'a -> ('a * 'b) option
```

## `.`

Separate components in a path.

```sml
val _ = Int.max (1, 5)
```

## `:`

1. Annotate an expression or pattern with a type.

   ```sml
   val a : int = 1 + 4
   val b = "hi" : string
   ```

2. Transparently ascribe a structure to a signature.

   ```sml
   structure S : sig
     type t
     val x : t
   end = struct
     type t = int
     val x = 5
   end

   val n : int = S.x + 3
   ```

## `;`

Separate elements in a sequence, like expressions.

```sml
val n : int = ("string"; false; 5)
```

Can also be used to separate declarations, but this is unnecessary.

Also indicates the end of input when in a REPL.

## `=`

1. Assign things.

   ```sml
   val a = 5
   fun f x = x + 4
   ```

2. Check for equality.

   ```sml
   fun isZero n = n = 0
   fun f s = if "foo" = s then 1 else 2
   ```

## `[`

Begin a list expression or pattern.

```sml
val xs = [1, 4, 9]
```

## `]`

The companion of `[`.

## `_`

Discard whatever matches the pattern.

```sml
val (_, a) = (4, "hi")
```

Also used as a expression/type "hole", though this is not legal SML.

## `{`

Begin a record expression or type.

```sml
val r = {a = 5, b = 9}
val {a, b} = r
```

## `|`

Separate arms in a matcher, constructors in a datatype, or cases in a `fun`.

Note that a leading `|` for the first case is not allowed.

```sml
val describe = fn
  0 => "nothing"
| 1 => "single"
| 2 => "dual"
| _ => "other"

datatype ans = Yes | No

fun toBool Yes = true
  | toBool No = false
```

## `}`

The companion of `{`.
