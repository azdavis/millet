signature XML_TREE = sig
  structure Schema : XML_SCHEMA
  datatype external_id = SYSTEM of string | PUBLIC of string * string
  datatype doctype = DOCTYPE of string * external_id option
  datatype content = TEXT of string | CDATA of string | ELEMENT of { name : Schema.element, attrs : Schema.attribute list, content : content list }
  type tree = { xmlDecl : Schema.attribute list option, doctype : doctype option, content : content }
end
