# Primitives

This is documentation for various SML primitive functions and types.

## `type int`

The built-in integer type.

Integers may be positive, negative, or 0. They are usually written with decimal digits.

```sml
val pos = 4
val neg = ~8
val zero = 0
```

They can also be written in hex, with the prefix `0x`.

```sml
val hex = 0x123beef
```

The `Int` structure provides operations on integers.

```sml
val three = Int.min (3, 5)
```

## `type word`

The built-in word type.

Words are unsigned integers. They are written in decimal digits with the prefix `0w`.

```sml
val dec = 0w123
```

They can also be written in hex with the prefix `0wx`.

```sml
val hex = 0wx123beef
```

The `Word` structure provides operations on words.

```sml
val nine = Word.max (0w4, 0w9)
```

## `type real`

The built-in real number type.

This type approximates the "mathematical" real numbers by using [floating-point arithmetic][fp].

```sml
val r = 1.4
val closeToZeroPoint3 = 0.1 + 0.2
```

Literals can have an exponent with an `e` or `E` following the base.

```sml
val withExp = 1.4e5
```

The `Real` structure provides operations on reals.

```sml
val fourPointSeven = Real.min (5.2, 4.7)
```

[fp]: https://0.30000000000000004.com

## `type char`

The built-in character type.

Literals are written like string literals (delimited with `"`), except they must:

- Have length 1.
- Have a leading `#`.

```sml
val percent = #"%"
val space = #" "
```

The `Char` structure provides operations on characters.

```sml
val no = Char.isSpace #"g"
```

The notion of "character" is difficult to define. C programmers may be used to "character" being synonymous with "byte", aka "octet", aka "8 bits", but this is not always the case.

See, for instance, the discussion on the Rust language's primitive [character type][rust-char], or the Unicode standard's [definition of "character"][unicode-char].

The relevant section of the Definition of Standard ML prescribes that the underlying encoding used in an SML implementation must agree with ASCII:

> We assume an underlying alphabet of N characters (N ≥ 256), numbered 0 to N − 1, which agrees with the ASCII character set on the characters numbered 0 to 127.

The common [UTF-8 encoding][utf8-everywhere] is one such encoding that has this property. Millet, implemented in Rust, requires SML source files be valid UTF-8, because [strings in Rust][rust-str] are required to always be valid UTF-8.

For maximum portability, however, an SML program should only use character literals that are ASCII, since the Definition prescribes no further restrictions on the underlying character set other than it must agree with ASCII.

[rust-char]: https://doc.rust-lang.org/stable/std/primitive.char.html
[unicode-char]: https://www.unicode.org/glossary/#character
[utf8-everywhere]: http://utf8everywhere.org
[rust-str]: https://doc.rust-lang.org/stable/std/primitive.str.html

## `type string`

The built-in string type.

Literals are delimited with double quotes, `"`.

```sml
val message = "hello, world!"
```

There are various string escapes available, all of which start with `\`.

```sml
val () = print "hi\n"
```

Strings can be concatenated with `^`.

```sml
val foobar = "foo" ^ "bar"
```

The `String` structure provides operations on strings.

```sml
val yes = String.isSubstring "erica" "america"
```

## `type bool`

The built-in boolean type.

There are two boolean values: `true` and `false`.

```sml
val yes = true
val no = false
```

Functions that check for equality or perform comparisons, like `=` and `>`, often return `bool`.

```sml
val yep = "foo" ^ "bar" = "foobar"
val nope = 3 > 7
```

`if` expressions case on the value of a `bool`.

```sml
fun choose (b : bool) = if b then "yea" else "nah"
```

The `Bool` structure provides operations on booleans.

```sml
val s = Bool.toString false
```

## `val true`

The `bool` that represents logical truth. Opposite of `false`.

## `val false`

The `bool` that represents logical falsity. Opposite of `true`.

## `type 'a list`

The built-in list type.

There are two constructors for lists:

- `nil`, the empty list.
- `::`, pronounced "cons", a non-empty list.

Cons takes an element `x` and a list `r` and returns the list that is that element `x` followed by `r`. `::` is an right-associative infix operator by default, so you may write `x :: r`.

```sml
val twoFourSix = 2 :: 4 :: 6 :: nil
```

Lists may be pattern-matched with `case` and similar constructs.

```sml
fun sum (xs : int list) : int =
  case xs of
    nil => 0
  | x :: r => x + sum r
```

There is syntax sugar for lists, written as:

- A `[` to start the list
- Comma-separated elements in the list
- A `]` to close the list

For example, all of these things are equivalent:

| Thing                    | What                         |
| ------------------------ | ---------------------------- |
| `[a, b, c]`              | Example list                 |
| `a :: b :: c :: nil`     | Desugared                    |
| `a :: (b :: (c :: nil))` | Explicit right associativity |

The `List` structure provides operations on lists.

```sml
val sizes : string list -> int list = List.map String.size
val three = List.length [2, 4, 6]
```

## `val nil`

The empty list constructor. Identical to `[]`.

## `val op ::`

The non-empty list constructor, pronounced "cons". Defaults to `infixr 5`.

## `type 'a ref`

The built-in reference type.

This is the core primitive for imperative programming in SML. It is one of the few types in SML whose values are mutable.

- To construct a ref, use `ref`.
- To get the current value in a ref, use `!`.
- To set the value in a ref to a new value, use `:=`.

```sml
val r = ref 0
val zero = !r
val () = r := 3
val three = !r
```

The `Ref` structure provides operations on references.

```sml
val r1 = ref 3
val r2 = ref 8
val () = Ref.swap (r1, r2)
val eight = !r1
val three = !r2
```

## `val ref`

The sole constructor for the built-in reference type.

## `type unit`

An alias for `{}`, the empty record/tuple type. It has one value, written `()` or equivalently `{}`. Useful for when a function is called only for its side effects and has no useful information to return, like `print`.

## `type exn`

The type of exceptions, which can be `raise`d or `handle`d.

## `val op *`

Multiplies two numbers.

## `val op +`

Adds two numbers.

## `val op -`

Subtracts the right hand number from the left.

## `val op /`

Divides the left hand real number by the right.

## `val op <`

Returns whether the left hand number, string, or character is less than the right.

## `val op <=`

Returns whether the left hand number, string, or character is less than or equal to the right.

## `val op >`

Returns whether the left hand number, string, or character is greater than the right.

## `val op >=`

Returns whether the left hand number, string, or character is greater than or equal to the right.

## `val op ~`

Negates the number.

## `val abs`

Returns the absolute value of the number.

## `val div`

Divides the left hand number by the right, rounding towards negative infinity.

## `val mod`

Computes the modulus of the left hand number with the right.

## `val op =`

Returns whether the two operands are equal.

## `val op <>`

Returns whether the two operands are unequal.

## `val use`

`use` is available in some SML implementations, like [SML/NJ][smlnj], but not others, like [MLton][].

When it is available, usually, the approximate semantics of calling `use f2` from a file `f1` is to "load" the contents of `f2`, and bring everything defined by that file `f2` into the scope of `f1`.

A small example:

<!-- @ignore generates warnings in Millet -->

```sml
(* f1.sml *)
val x = 3
(* f2.sml *)
val () = use "f1.sml"
val y = x + 4
```

`use` is sometimes used for small-scale multi-file SML projects. For larger projects, consider:

- [SML/NJ Compilation Manager][cm]
- [ML Basis][mlb]

[smlnj]: https://www.smlnj.org
[mlton]: http://mlton.org
[cm]: https://www.smlnj.org/doc/CM/new.pdf
[mlb]: http://mlton.org/MLBasis
