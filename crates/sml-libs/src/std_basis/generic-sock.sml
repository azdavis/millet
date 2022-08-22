(*!
 * Implementations may provide the GenericSock structure as a way to provide access to additional
 * address families and socket types (beyond those supported by INetSock and UnixSock).
 *)
signature GENERIC_SOCK (* OPTIONAL *) = sig
  (*!
   * socket (af, st) creates a socket in the address family specified by af and the socket type
   * specified by st, with the default protocol.
   *)
  val socket : Socket.AF.addr_family * Socket.SOCK.sock_type -> ('af, 'sock_type) Socket.sock
  (*!
   * socketPair (af, st) creates an unnamed pair of connected sockets in the address family
   * specified by af and the socket type specified by st, with the default protocol.
   *)
  val socketPair : Socket.AF.addr_family * Socket.SOCK.sock_type -> ('af, 'sock_type) Socket.sock * ('af, 'sock_type) Socket.sock
  (*!
   * socket' (af, st, i) creates a socket in the address family specified by af and the socket type
   * specified by st, with protocol number i.
   *)
  val socket' : Socket.AF.addr_family * Socket.SOCK.sock_type * int -> ('af, 'sock_type) Socket.sock
  (*!
   * socketPair' (af, st, i) creates an unnamed pair of connected sockets in the address family
   * specified by af and the socket type specified by st, with protocol number i.
   *)
  val socketPair' : Socket.AF.addr_family * Socket.SOCK.sock_type * int -> ('af, 'sock_type) Socket.sock * ('af, 'sock_type) Socket.sock
end

structure GenericSock :> GENERIC_SOCK (* OPTIONAL *) = struct end
