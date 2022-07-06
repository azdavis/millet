signature SOCK_UTIL = sig
  datatype port = PortNumber of int | ServName of string
  datatype hostname = HostName of string | HostAddr of NetHostDB.in_addr
  val scanAddr : (char, 'a) StringCvt.reader -> ({host : hostname, port : port option}, 'a) StringCvt.reader
  val addrFromString : string -> {host : hostname, port : port option} option
  exception BadAddr of string
  val resolveAddr : {host : hostname, port : port option} -> {host : string, addr : NetHostDB.in_addr, port : int option}
  type 'a stream_sock = ('a, Socket.active Socket.stream) Socket.sock
  val connectINetStrm : {addr : NetHostDB.in_addr, port : int} -> INetSock.inet stream_sock
  val recvVec : ('a stream_sock * int) -> Word8Vector.vector
  val recvStr : ('a stream_sock * int) -> string
  val sendVec : ('a stream_sock * Word8Vector.vector) -> unit
  val sendStr : ('a stream_sock * string) -> unit
  val sendArr : ('a stream_sock * Word8Array.array) -> unit
end

structure SockUtil : SOCK_UTIL = struct end
