signature SIG = sig
  type t
end
structure S: SIG = struct
  type t = int
end
