signature REGEXP_ENGINE = sig
  type regexp
  type 'a match = {pos : 'a, len : int} MatchTree.match_tree
  val compile : RegExpSyntax.syntax -> regexp
  val find : regexp -> (char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
  val prefix : regexp ->(char,'a) StringCvt.reader -> ('a match, 'a) StringCvt.reader
  val match : (RegExpSyntax.syntax * ('a match -> 'b)) list -> (char,'a) StringCvt.reader -> ('b, 'a) StringCvt.reader
end

structure BackTrackEngine : REGEXP_ENGINE = struct end
structure DfaEngine : REGEXP_ENGINE = struct end
structure ThompsonEngine : REGEXP_ENGINE = struct end
