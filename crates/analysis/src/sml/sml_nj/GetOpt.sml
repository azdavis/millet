structure GetOpt : sig
  datatype 'a arg_order = RequireOrder | Permute | ReturnInOrder of string -> 'a
  datatype 'a arg_descr = NoArg of unit -> 'a | ReqArg of (string -> 'a) * string | OptArg of (string option -> 'a) * string
  type 'a opt_descr = { short : string, long : string list, desc : 'a arg_descr, help : string }
  val usageInfo : { header : string, options : 'a opt_descr list } -> string
  val getOpt : { argOrder : 'a arg_order, options : 'a opt_descr list, errFn : string -> unit } -> string list -> ('a list * string list)
end = struct end
