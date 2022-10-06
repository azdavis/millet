signature FORMAT_COMB = sig
  type 'a format
  type ('a, 'b) fragment = 'a format -> 'b format
  type 'a glue = ('a, 'a) fragment
  type ('a, 't) element = ('a, 't -> 'a) fragment
  type 'a gg
  val format : (string, 'a) fragment -> 'a
  val format' : (string list -> 'b) -> ('b, 'a) fragment -> 'a
  val using : ('t -> string) -> ('a, 't) element
  val int : ('a, int) element
  val real : ('a, real) element
  val bool : ('a, bool) element
  val string : ('a, string) element
  val string' : ('a, string) element
  val char : ('a, char) element
  val char' : ('a, char) element
  val int' : StringCvt.radix -> ('a, int) element
  val real' : StringCvt.realfmt -> ('a, real) element
  val list : ('a, 'x) element -> ('a, 'x list) element
  val option : ('a, 'x) element -> ('a, 'x option) element
  val seq : (('x * 'a gg -> 'a gg) -> 'a gg -> 's -> 'a gg) -> 'a glue -> ('a, 'x) element -> ('a, 's) element
  val glue : ('a, 't) element -> 't -> 'a glue
  val elem : ('t -> 'a glue) -> ('a, 't) element
  val nothing : 'a glue
  val text : string -> 'a glue
  val sp : int -> 'a glue
  val nl : 'a glue
  val tab : 'a glue
  val listg : ('t -> 'a glue) -> ('t list -> 'a glue)
  val optiong : ('t -> 'a glue) -> ('t option -> 'a glue)
  val seqg : (('x * 'a gg -> 'a gg) -> 'a gg -> 's -> 'a gg) -> 'a glue -> ('x -> 'a glue) -> 's -> 'a glue
  type place
  val left : place
  val center : place
  val right : place
  val pad : place -> int -> ('a, 't) fragment -> ('a, 't) fragment
  val trim : place -> int -> ('a, 't) fragment -> ('a, 't) fragment
  val fit : place -> int -> ('a, 't) fragment -> ('a, 't) fragment
  val padl : int -> ('a, 't) fragment -> ('a, 't) fragment
  val padr : int -> ('a, 't) fragment -> ('a, 't) fragment
end

structure FormatComb : FORMAT_COMB = struct end
