# Keywords

This is documentation for all the keywords in SML, and what they mean.

If a SML keywords is not documented here, please file an issue.

## `exception`

Begins an exception declaration.

```sml
exception Foo
exception Bar of string
```

Exception declarations define new exceptions.

## `signature`

Begins a signature declaration.

```sml
signature SIG = sig
  type t
  val x : t
end
```

Signatures describe the interface to structures.

## `structure`

Begins a structure declaration.

```sml
structure Str = struct
  val x = 3
  val y = "hi"
end
```

Structures are collections of declarations.

## `datatype`

Begins a datatype declaration.

```sml
datatype answer = Yes | No | Maybe
datatype debug = Bool of bool | Level of int
```

Datatype declarations define new types and their constructors.

## `withtype`

Used with `datatype` (and `abstype`) to add extra helper types into scope at the same time as the datatype. This allows the types to be used in the value constructors.

```sml
datatype 'a stream = Nil | Cons of 'a * 'a front
withtype 'a front = unit -> 'a stream
```

## `abstype`

Begins an abstype declaration.

This is not really used in modern SML. Prefer a mix of:

- `datatype`, for creating new types
- `structure`, for managing collections of declarations
- `signature` and ascription, for hiding implementation details

## `andalso`

An infix keyword that computes logical "and" with two `bool` expressions.

```sml
fun canRide height age =
  height > 6 andalso age >= 18
```

It short-circuits, so if the first expression evaluates to `false`, the second is not evaluated.

Because of this special short-circuiting behavior, it is not a regular infix operator, and thus does not work with `op`.

Not to be confused with `and`, which permits declaring multiple things at once.

## `functor`

Begins a functor declaration.

```sml
functor F (A : sig
  val x : int
end) = struct
  val y = A.x + 123
end
```

Functors are structure-level functions. To wit:

- Regular functions take in values and return values.
- Functors take in structures and return structures.

## `include`

Begins an include specification, for use in signatures.

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

Including a signature in another signature "merges" the two signatures together.

## `sharing`

Used in signature specifications to indicate type sharing.

This is a fairly advanced SML feature.

## `eqtype`

Like `type`, but can only be used in signature specifications. It indicates the type must be an equality type.

```sml
signature SIG = sig
  eqtype t
  val x : t
end
```

## `handle`

Handle a set of exceptions.

```sml
val _ = 3 + 4 handle Overflow => 0
```

The "`handle` clause", which is the `handle` keywords plus the matcher listing the handled exceptions, is added to the end of an expression (the "head" expression).

- If the head expression **does not** raise, the whole expression (i.e. the head expression plus the `handle` clause) evaluates to whatever the head expression did.
- If the head expression **does** raise, and the exception raised **is** matched by the matcher, the whole expression evaluates to the expression in that corresponding matcher arm.
- If the head expression **does** raise, and the exception raised **is not** matched, the whole expression raises.

## `infixr`

Begins a fixity declaration, that causes the given names to be infix operators with the given precedence (or 0 if none is provided), and right associativity. Similar to `infix`.

```sml
infixr foo
infixr 3 bar quz
```

## `nonfix`

Begins a fixity declaration, that causes the given names to not be infix.

```sml
nonfix foo
nonfix bar quz
```

## `orelse`

An infix keyword that computes logical "or" with two `bool` expressions.

```sml
fun canAfford price persuasiveness =
  price <= 3 orelse persuasiveness >= 75
```

It short-circuits, so if the first expression evaluates to `true`, the second is not evaluated.

Because of this special short-circuiting behavior, it is not a regular infix operator, and thus does not work with `op`.

## `struct`

Begins a structure expression.

```sml
structure S = struct
  val x = 5
end
```

Structure expressions are often used as

- The right-hand side to structure declarations.
- The arguments to functors.

See `structure` and `functor`.

## `infix`

Begins a fixity declaration, that causes the given names to be infix operators with the given precedence (or 0 if none is provided), and left associativity. Similar to `infixr`.

```sml
infix foo
infix 3 bar quz
```

## `local`

Begins a local declaration.

```sml
local
  val x = 5
in
  val y = x + 4
  val z = x + 9
end
```

Declarations in the `local ... in` may be used by declarations in the `in ... end`, but only the latter declarations are in scope outside of the local.

## `raise`

Begins a raise expression.

```sml
fun fact n =
  if n < 0 then
    raise Fail "can't do factorial of a negative"
  else if n = 0 then
    1
  else
    n * fact (n - 1)
```

`raise` takes an exception expression (one of type `exn`), evaluates it, and raises that exception. A raised exception may be handled with `handle`.

## `where`

Used in signature expressions to indicate type sharing.

This is a fairly advanced SML feature.

## `while`

Execute a loop body while a condition is upheld.

```sml
fun go () =
  let
    val r = ref 0
  in
    while !r < 5 do (r := !r + 1; print "hi")
  end
```

This is an "imperative" SML feature that requires the use of `ref` or other side effects to be useful.

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
fun choose x = if x then "yes" else "no"
```

## `open`

Brings a structure's members into scope.

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
fun choose x = if x then "yes" else "no"
```

## `type`

Begins a type declaration.

```sml
type point = int * int
```

Types defined with `type` are aliases, meaning no "new" types are defined. Rather, the new name is just an alternative name for the right-hand-side type, and aside from name, they are equivalent.

## `with`

Used in abstype declarations, which is to say, not much.

## `and`

Used to declare many things simultaneously. Most useful for mutually recursive functions.

```sml
fun even 0 = true
  | even n = odd (n - 1)
and odd 0 = false
  | odd n = even (n - 1)
```

Not to be confused with `andalso`, which is for logical "and" of `bool` expressions.

## `end`

Marks the end of various constructs, like:

- `let ... in ... end`
- `local ... in ... end`
- `struct ... end`
- `sig ... end`

## `fun`

Begins a function declaration.

```sml
fun inc x = x + 1
```

Functions are the unit of abstraction.

Not to be confused with `fn`, which begins a function (aka lambda) expression.

## `let`

Begins a let expression.

```sml
val _ =
  let
    val x = 7
    val y = 5
  in
    x + y
  end
```

- In the `let ... in`, there are declarations.
- In the `in ... end`, there is an expression (or a sequence).
- The `let` first defines the declarations in the `let ... in`, then evaluates the expressions in the `in ... end` in sequence.
- The expressions may use bindings from the declarations.
- The value of the whole `let` is the value of the last expression in the `in ... end`.

## `rec`

Denotes a recursive lambda.

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

Begins a signature expression.

```sml
signature SIG = sig
  type t
  val x : t
end
```

Signature expressions are often used as

- The right-hand side to signature declarations.
- The interfaces to functors.

## `val`

Begins a value declaration.

```sml
val y = 6
```

A value declaration evaluates the expression on the right, then matches it with the pattern on the left, and introduces new bindings produced by the pattern.

## `as`

Used with patterns to match the entire value to a variable.

```sml
fun f [] = []
  | f (xs as (_ :: xs')) = xs @ f xs'
```

The syntax is `<name> as <pat>`.

## `do`

Separates a `while` loop condition from the body.

```sml
val _ = while true do print "y\n"
```

## `fn`

Begins a function ("lambda") expression.

```sml
val _ = List.map (fn x => x + 1) [1, 3, 8]
```

Often used as the argument to higher-order functions, to avoid having to declare and name small "helper" functions.

## `if`

Begins an if expression.

```sml
val _ = if 3 > 4 then "math is broken" else "okay good"
```

An if expression cases on a condition of type `bool`, and selects either the `then` expression if the condition was `true`, or the `else` if it was `false`.

## `in`

Separates the first and second parts of a `local` declaration or `let` expression.

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

This keyword fulfills two distinct functions:

1. Separates the head expression of a `case` from the arms.

   ```sml
   fun hm x =
     case x of
       1 => 2
     | _ => 3
   ```

2. Denotes that a value or exception constructor takes an argument.

   ```sml
   datatype ans = No | Yes | Custom of string
   ```

## `op`

Causes an infix identifier to temporarily act as though it is not.

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
