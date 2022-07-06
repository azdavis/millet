signature REGEXP_SYNTAX = sig
  exception CannotCompile
  structure CharSet : ORD_SET where type Key.ord_key = char
  datatype syntax = Group of syntax | Alt of syntax list | Concat of syntax list | Interval of (syntax * int * int option) | MatchSet of CharSet.set | NonmatchSet of CharSet.set | Char of char | Begin | End
  val optional : syntax -> syntax
  val closure : syntax -> syntax
  val posClosure : syntax -> syntax
  val fromRange : char * char -> CharSet.set
  val addRange : CharSet.set * char * char -> CharSet.set
  val allChars : CharSet.set
  val alnum : CharSet.set
  val alpha : CharSet.set
  val ascii : CharSet.set
  val blank : CharSet.set
  val cntl : CharSet.set
  val digit : CharSet.set
  val graph : CharSet.set
  val lower : CharSet.set
  val print : CharSet.set
  val punct : CharSet.set
  val space : CharSet.set
  val upper : CharSet.set
  val word : CharSet.set
  val xdigit : CharSet.set
end

structure RegExpSyntax : REGEXP_SYNTAX = struct end
