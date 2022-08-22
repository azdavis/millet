(*!
 * This structure accesses the information contained in the network protocol data base. The data may
 * be retrieved from a file, such as /etc/protocols on many Unix systems, or via the NIS protocols
 * map.
 *)
signature NET_PROT_DB (* OPTIONAL *) = sig
  (*!
   * The type of a network protocol data base entry.
   *)
  type entry
  (*!
   * name en returns the official name of the protocol described by entry en (e.g., "ip").
   *)
  val name : entry -> string
  (*!
   * aliases en returns the alias list of the protocol described by entry en.
   *)
  val aliases : entry -> string list
  (*!
   * protocol en returns the protocol number of the protocol described by entry en.
   *)
  val protocol : entry -> int
  (*!
   * getByName s reads the network protocol data base for a protocol with name s. If successful, it
   * returns SOME(en) where en is the corresponding data base entry; otherwise, it returns NONE.
   *)
  val getByName : string -> entry option
  (*!
   * getByNumber i reads the network protocol data base for a protocol with protocol number i. If
   * successful, it returns SOME(en) where en is the corresponding data base entry; otherwise, it
   * returns NONE.
   *)
  val getByNumber : int -> entry option
end

structure NetProtDB :> NET_PROT_DB (* OPTIONAL *) = struct end
