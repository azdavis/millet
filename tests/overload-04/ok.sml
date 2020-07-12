(* abs *)
val _: int = abs 1
val _: real = abs 1.1
(* tilde. put a space so the ~ is a function, not part of the constant. *)
val _: int = ~ 1
val _: real = ~ 1.1
(* div *)
val _: int = 1 div 1
val _: word = 0w0 div 0w0
(* mod *)
val _: int = 1 mod 1
val _: word = 0w0 mod 0w0
(* star *)
val _: int = 1 * 1
val _: word = 0w0 * 0w0
val _: real = 1.1 * 1.1
(* slash *)
val _: real = 1.1 / 1.1
(* plus *)
val _: int = 1 + 1
val _: word = 0w0 + 0w0
val _: real = 1.1 + 1.1
(* minus *)
val _: int = 1 - 1
val _: word = 0w0 - 0w0
val _: real = 1.1 - 1.1
(* lt *)
val _: bool = 1 < 1
val _: bool = 0w0 < 0w0
val _: bool = 1.1 < 1.1
val _: bool = "e" < "e"
val _: bool = #"e" < #"e"
(* lt eq *)
val _: bool = 1 <= 1
val _: bool = 0w0 <= 0w0
val _: bool = 1.1 <= 1.1
val _: bool = "e" <= "e"
val _: bool = #"e" <= #"e"
(* gt *)
val _: bool = 1 > 1
val _: bool = 0w0 > 0w0
val _: bool = 1.1 > 1.1
val _: bool = "e" > "e"
val _: bool = #"e" > #"e"
(* gt eq *)
val _: bool = 1 >= 1
val _: bool = 0w0 >= 0w0
val _: bool = 1.1 >= 1.1
val _: bool = "e" >= "e"
val _: bool = #"e" >= #"e"
