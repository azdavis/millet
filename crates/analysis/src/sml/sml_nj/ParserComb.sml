signature PARSER_COMB = sig
  type ('a, 'strm) parser = (char, 'strm) StringCvt.reader -> ('a, 'strm) StringCvt.reader
  val result : 'a -> ('a, 'strm) parser
  val failure : ('a, 'strm) parser
  val wrap : (('a, 'strm) parser * ('a -> 'b)) -> ('b, 'strm) parser
  val seq : (('a, 'strm) parser * ('b, 'strm) parser) -> (('a * 'b), 'strm) parser
  val seqWith : (('a * 'b) -> 'c) -> (('a, 'strm) parser * ('b, 'strm) parser) -> ('c, 'strm) parser
  val bind : (('a, 'strm) parser * ('a -> ('b, 'strm) parser)) -> ('b, 'strm) parser
  val eatChar : (char -> bool) -> (char, 'strm) parser
  val char : char -> (char, 'strm) parser
  val string : string -> (string, 'strm) parser
  val skipBefore : (char -> bool) -> ('a, 'strm) parser -> ('a, 'strm) parser
  val or : (('a, 'strm) parser * ('a, 'strm) parser) -> ('a, 'strm) parser
  val or' : ('a, 'strm) parser list -> ('a, 'strm) parser
  val zeroOrMore : ('a, 'strm) parser -> ('a list, 'strm) parser
  val oneOrMore : ('a, 'strm) parser -> ('a list, 'strm) parser
  val option : ('a, 'strm) parser -> ('a option, 'strm) parser
  val join : ('a option, 'strm) parser -> ('a, 'strm) parser
  val token : (char -> bool) -> (string, 'strm) parser
end

structure ParserComb : PARSER_COMB = struct end
