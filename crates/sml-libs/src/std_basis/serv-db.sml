(*!
 * This structure accesses the information contained in the network services data base. This data
 * may be retrieved from the file /etc/services on many Unix systems, or from some other data base.
 *)
signature NET_SERV_DB (* OPTIONAL *) = sig
  (*!
   * The abstract type of a network service database entry.
   *)
  type entry
  (*!
   * name ent returns the official name of the service described by entry ent (e.g., "ftp",
   * "telnet", etc.).
   *)
  val name : entry -> string
  (*!
   * aliases ent returns the alias list of the service described by entry ent.
   *)
  val aliases : entry -> string list
  (*!
   * port ent returns the port number of the service described by entry ent.
   *)
  val port : entry -> int
  (*!
   * protocol ent returns the name of the protocol to use for the service described by the entry ent
   * (e.g., "tcp" or "udp").
   *)
  val protocol : entry -> string
  (*!
   * getByName (s, prot) reads the network service data base for a service with name s. If prot is
   * SOME(protname), the protocol of the service must also match protname; if prot is NONE, no
   * protocol restriction is imposed. If successful, it returns SOME(en) where en is the
   * corresponding data base entry; otherwise, it returns NONE.
   *)
  val getByName : string * string option -> entry option
  (*!
   * getByPort (i, prot) reads the network service data base for a service with port number i. If
   * prot is SOME(protname), the protocol of the service must also match protname; if prot is NONE,
   * no protocol restriction is imposed. If successful, it returns SOME(en) where en the
   * corresponding data base entry; otherwise, it returns NONE.
   *)
  val getByPort : int * string option -> entry option
end

structure NetServDB :> NET_SERV_DB (* OPTIONAL *) = struct end
