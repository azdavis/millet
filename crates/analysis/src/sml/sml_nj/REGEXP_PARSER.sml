signature REGEXP_PARSER = sig
  val scan : (char, 'a) StringCvt.reader -> (RegExpSyntax.syntax, 'a) StringCvt.reader
end
structure AwkSyntax : REGEXP_PARSER = struct end
