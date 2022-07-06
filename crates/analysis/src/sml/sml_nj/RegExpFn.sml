signature REGEXP = sig
  type regexp
  type 'a match = {pos : 'a, len : int} MatchTree.match_tree
  exception CannotParse
  val compile : (char,'a) StringCvt.reader -> (regexp, 'a) StringCvt.reader
  val compileString : string -> regexp
  val find : regexp -> (char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
  val prefix : regexp -> (char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
  val match : (string * ('a match -> 'b)) list -> (char,'a) StringCvt.reader -> ('b, 'a) StringCvt.reader
end

functor RegExpFn (
  structure P : REGEXP_PARSER
  structure E : REGEXP_ENGINE
) :> REGEXP where type regexp = E.regexp = struct end
