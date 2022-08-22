(*!
 * This structure accesses the information contained in the network host data base. The data might
 * be retrieved from a file such as /etc/hosts on Unix systems, or dynamically via some network
 * communication. The structure can be used to convert host names (e.g., "cs.princeton.edu") to
 * Internet addresses (e.g., "128.112.136.10").
 *)
signature NET_HOST_DB (* OPTIONAL *) = sig
  (*!
   * The type representing an Internet address.
   *)
  eqtype in_addr
  (*!
   * The type representing address families (also known as domains).
   *)
  eqtype addr_family
  (*!
   * The type representing an entry from the host database.
   *)
  type entry
  (*!
   * name en returns the official name of the host described by entry en.
   *)
  val name : entry -> string
  (*!
   * aliases en returns the alias list of the host described by entry en.
   *)
  val aliases : entry -> string list
  (*!
   * addrType en returns the address family of the host described by entry en.
   *)
  val addrType : entry -> addr_family
  (*!
   * addr en returns the main Internet address of the host described by entry en. This is the first
   * address of the list returned by addrs.
   *)
  val addr : entry -> in_addr
  (*!
   * addrs en returns the list of Internet addresses of the host described by entry en. The list is
   * guaranteed to be non-empty.
   *)
  val addrs : entry -> in_addr list
  (*!
   * getByName s reads the network host data base for a host with name s. If successful, it returns
   * SOME(en) where en is the corresponding data base entry; otherwise, it returns NONE.
   *)
  val getByName : string -> entry option
  (*!
   * getByAddr ia reads the network host data base for a host with Internet address ia. If
   * successful, it returns SOME(en) where en is the corresponding data base entry; otherwise, it
   * returns NONE.
   *)
  val getByAddr : in_addr -> entry option
  (*!
   * The standard hostname for the current processor.
   *)
  val getHostName : unit -> string
  (*!
   * toString ia returns a string representation of the Internet address ia in the form "a.b.c.d".
   *)
  val toString : in_addr -> string
  (*!
   * These functions scan Internet address from a character source. The first returns SOME(ia,rest)
   * if an Internet address can be parsed from a prefix of the character stream strm after skipping
   * initial whitespace. ia is the resulting address, and rest is the remainder of the character
   * stream. NONE is returned otherwise. The second form returns SOME(ia) if an Internet address ia
   * can be parsed from a prefix of string s. NONE is returned otherwise. It is equivalent to
   * StringCvt.scanString scan. Addresses in this notation have one of the following forms: a where
   * a is a 32-bit unsigned integer constant. a.b where a is an 8-bit unsigned integer constant, and
   * b is a 24-bit integer constant. a.b.c where a and b are 8-bit unsigned integer constants, and c
   * is a 16-bit integer constant. a.b.c.d where a, b, c, and d are 8-bit integer constants. The
   * integer constants may be decimal, octal, or hexadecimal, as specified in the C language.
   *)
  val scan : (char, 'a) StringCvt.reader -> (in_addr, 'a) StringCvt.reader
  (*!
   * See scan.
   *)
  val fromString : string -> in_addr option
end

structure NetHostDB :> NET_HOST_DB (* OPTIONAL *) = struct end
