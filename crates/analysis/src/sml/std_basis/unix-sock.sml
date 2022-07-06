(*!
This structure is used to create sockets in the Unix address family. This structure is only present
when the underlying operating system supports Unix-domain sockets.

Binding a name to a Unix-domain socket with bind causes a socket file to be created in the
filesystem. This file is not removed when the socket is closed; OS.FileSys.remove must be used to
remove the file. The usual filesystem access-control mechanisms are applied when referencing
Unix-domain sockets; e.g., the file representing the destination of a connect or sendVec must be
writable.
!*)
signature UNIX_SOCK (* OPTIONAL *) = sig
  (*!
  The witness type of the Unix address family.
  !*)
  type unix
  (*!
  The type-scheme for all Unix-domain sockets.
  !*)
  type 'sock_type sock = (unix, 'sock_type) Socket.sock
  (*!
  The type-scheme of Unix-domain (passive or active) stream sockets.
  !*)
  type 'mode stream_sock = 'mode Socket.stream sock
  (*!
  The type of Unix-domain datagram sockets.
  !*)
  type dgram_sock = Socket.dgram sock
  (*!
  The type of a Unix-domain socket address.
  !*)
  type sock_addr = unix Socket.sock_addr
  (*!
  The Unix address family value.
  !*)
  val unixAF : Socket.AF.addr_family
  (*!
  toAddr s converts a pathname s into a socket address (in the Unix address family); it does not
  check the validity of the path s.
  !*)
  val toAddr : string -> sock_addr
  (*!
  fromAddr addr returns the Unix file system path corresponding to the Unix-domain socket address
  addr.
  !*)
  val fromAddr : sock_addr -> string
  structure Strm : sig
    (*!
    This function creates a stream socket in the Unix address family. It raises SysErr if there are
    too many sockets in use.
    !*)
    val socket : unit -> 'mode stream_sock
    (*!
    This function creates an unnamed pair of connected stream sockets in the Unix address family. It
    is similar to the Posix.IO.pipe function in that the returned sockets are connected, but unlike
    pipe, the sockets are bidirectional. It raises SysErr if there are too many sockets in use.
    !*)
    val socketPair : unit -> 'mode stream_sock * 'mode stream_sock
  end
  structure DGrm : sig
    (*!
    This function creates a datagram socket in the Unix address family. It raises SysErr if there
    are too many sockets in use.
    !*)
    val socket : unit -> dgram_sock
    (*!
    This function creates an unnamed pair of connected datagram sockets in the Unix address family.
    It raises SysErr if there are too many sockets in use.
    !*)
    val socketPair : unit -> dgram_sock * dgram_sock
  end
end

structure UnixSock :> UNIX_SOCK (* OPTIONAL *) = struct end
