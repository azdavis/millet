structure GenericXMLTree : XML_TREE
  where type Schema.element = Atom.atom
  where type Schema.attribute = Atom.atom * string
  = struct end
