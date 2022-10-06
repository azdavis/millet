signature XML_PARSER = sig
  structure XMLTree : XML_TREE
  val parseFile : string -> XMLTree.tree
  exception ParseError of string
end

functor XMLParserFn (XT : XML_TREE) : XML_PARSER = struct end
