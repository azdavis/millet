(*!
This structure provides operations for creating and manipulating Internet-domain addresses and
sockets.
!*)
signature INET_SOCK (* OPTIONAL *) = sig
  (*!
  The witness type of the INet address family.
  !*)
  type inet
  type 'sock_type sock = (inet, 'sock_type) Socket.sock
  (*!
  The type-scheme of Internet-domain stream sockets; The type parameter 'mode can be instantiated to
  either Socket.active or Socket.passive.
  !*)
  type 'mode stream_sock = 'mode Socket.stream sock
  (*!
  The type of Internet-domain datagram sockets.
  !*)
  type dgram_sock = Socket.dgram sock
  (*!
  The type of Internet-domain socket addresses.
  !*)
  type sock_addr = inet Socket.sock_addr
  (*!
  The address family value that represents the Internet domain.
  !*)
  val inetAF : Socket.AF.addr_family
  (*!
  toAddr (ia, i) converts an Internet address ia and a port number i into a socket address (in the
  INet address family).
  !*)
  val toAddr : NetHostDB.in_addr * int -> sock_addr
  (*!
  This function converts a socket address (in the INet address family) into a pair (ia,i) of an
  Internet address ia and a port number i.
  !*)
  val fromAddr : sock_addr -> NetHostDB.in_addr * int
  (*!
  any port creates a socket address that fixes the port to port, but leaves the Internet address
  unspecified. This function corresponds to the INADDR_ANY constant in the C Sockets API. The values
  created by this function are used to bind a socket to a specific port.
  !*)
  val any : int -> sock_addr
  structure UDP : sig
    (*!
    This creates a datagram socket in the INet address family with the default protocol. It raises
    SysErr if there are too many sockets in use.
    !*)
    val socket : unit -> dgram_sock
    (*!
    socket' prot creates a datagram socket in the INet address family with the protocol number prot.
    The interpretation of prot is system dependent, but a value of 0 is equivalent to socket(). It
    raises SysErr if there are too many sockets in use.
    !*)
    val socket' : int -> dgram_sock
  end
  structure TCP : sig
    (*!
    This creates a stream socket in the INet address family with the default protocol. It raises
    SysErr if there are too many sockets in use.
    !*)
    val socket : unit -> 'mode stream_sock
    (*!
    socket' prot creates a stream socket in the INet address family with the protocol number prot.
    The interpretation of prot is system dependent, but a value of 0 is equivalent to socket().
    !*)
    val socket' : int -> 'mode stream_sock
    (*!
    These functions query and set the TCP_NODELAY flag on the socket. When set to false (the
    default), there is only a single small packet allowed to be outstanding on a given TCP
    connection at any time, thereby reducing small packet traffic on slower WANs. When set to true,
    packets are sent as fast as possible. [[Refer to Stevens, p.316.]]
    !*)
    val getNODELAY : 'mode stream_sock -> bool
    val setNODELAY : 'mode stream_sock * bool -> unit
  end
end

structure INetSock :> INET_SOCK (* OPTIONAL *) = struct end
