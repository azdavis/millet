signature SOCKET (* OPTIONAL *) = sig
  type ('af,'sock_type) sock
  type 'af sock_addr
  type dgram
  type 'mode stream
  type passive
  type active
  structure AF : sig
    type addr_family = NetHostDB.addr_family
    val list : unit -> (string * addr_family) list
    val toString : addr_family -> string
    val fromString : string -> addr_family option
  end
  structure SOCK : sig
    eqtype sock_type
    val stream : sock_type
    val dgram : sock_type
    val list : unit -> (string * sock_type) list
    val toString : sock_type -> string
    val fromString : string -> sock_type option
  end
  structure Ctl : sig
    val getDEBUG : ('af, 'sock_type) sock -> bool
    val setDEBUG : ('af, 'sock_type) sock * bool -> unit
    val getREUSEADDR : ('af, 'sock_type) sock -> bool
    val setREUSEADDR : ('af, 'sock_type) sock * bool -> unit
    val getKEEPALIVE : ('af, 'sock_type) sock -> bool
    val setKEEPALIVE : ('af, 'sock_type) sock * bool -> unit
    val getDONTROUTE : ('af, 'sock_type) sock -> bool
    val setDONTROUTE : ('af, 'sock_type) sock * bool -> unit
    val getLINGER : ('af, 'sock_type) sock -> Time.time option
    val setLINGER : ('af, 'sock_type) sock * Time.time option -> unit
    val getBROADCAST : ('af, 'sock_type) sock -> bool
    val setBROADCAST : ('af, 'sock_type) sock * bool -> unit
    val getOOBINLINE : ('af, 'sock_type) sock -> bool
    val setOOBINLINE : ('af, 'sock_type) sock * bool -> unit
    val getSNDBUF : ('af, 'sock_type) sock -> int
    val setSNDBUF : ('af, 'sock_type) sock * int -> unit
    val getRCVBUF : ('af, 'sock_type) sock -> int
    val setRCVBUF : ('af, 'sock_type) sock * int -> unit
    val getTYPE : ('af, 'sock_type) sock -> SOCK.sock_type
    val getERROR : ('af, 'sock_type) sock -> bool
    val getPeerName : ('af, 'sock_type) sock -> 'af sock_addr
    val getSockName : ('af, 'sock_type) sock -> 'af sock_addr
    val getNREAD : ('af, 'sock_type) sock -> int
    val getATMARK : ('af, active stream) sock -> bool
  end
  (*!
  This tests whether two socket addresses are the same address.
  !*)
  val sameAddr : 'af sock_addr * 'af sock_addr -> bool
  (*!
  familyOfAddr addr returns the address family of the socket address addr.
  !*)
  val familyOfAddr : 'af sock_addr -> AF.addr_family
  (*!
  bind (sock, sa) binds the address sa to the passive socket sock. This function raises SysErr when
  the address sa is already in use, when sock is already bound to an address, or when sock has been
  closed.
  !*)
  val bind : ('af, 'sock_type) sock * 'af sock_addr -> unit
  (*!
  listen (sock, n) creates a queue (of size n) for pending questions associated to the socket sock.
  The size of queue is limited by the underlying system, but requesting a queue size larger than the
  limit does not cause an error (a typical limit is 128, but older systems use a limit of 5). This
  function raises the SysErr exception if sock has been closed.
  !*)
  val listen : ('af, passive stream) sock * int -> unit
  (*!
  accept sock extracts the first connection request from the queue of pending connections for the
  socket sock. The socket must have been bound to an address via bind and enabled for listening via
  listen. If a connection is present, accept returns a pair (s,sa) consisting of a new active socket
  s with the same properties as sock and the address sa of the connecting entity. If no pending
  connections are present on the queue then accept blocks until a connection is requested. One can
  test for pending connection requests by using the select function to test the socket for reading.
  This function raises the SysErr exception if sock has not been properly bound and enabled, or it
  sock has been closed.
  !*)
  val accept : ('af, passive stream) sock -> ('af, active stream) sock * 'af sock_addr
  (*!
  This function is the nonblocking form of the accept operation. If the operation can complete
  without blocking (i.e., there is a pending connection), then this function returns SOME(s,sa),
  where s is a new active socket with the same properties as sock and sa is the the address of the
  connecting entity. If there are no pending connections, then this function returns NONE. This
  function raises the SysErr exception if sock has not been properly bound and enabled, or it sock
  has been closed.
  !*)
  val acceptNB : ('af, passive stream) sock -> (('af, active stream) sock * 'af sock_addr) option
  (*!
  connect (sock, sa) attempts to connect the socket sock to the address sa. If sock is a datagram
  socket, the address specifies the peer with which the socket is to be associated; sa is the
  address to which datagrams are to be sent, and the only address from which datagrams are to be
  received. If sock is a stream socket, the address specifies another socket to which to connect.
  This function raises the SysErr exception when the address specified by sa is unreachable, when
  the connection is refused or times out, when sock is already connected, or when sock has been
  closed.
  !*)
  val connect : ('af, 'sock_type) sock * 'af sock_addr -> unit
  (*!
  This function is the nonblocking form of connect. If the connection can be established without
  blocking the caller (which is typically true for datagram sockets, but not stream sockets), then
  true is returned. Otherwise, false is returned and the connection attempt is started; one can test
  for the completion of the connection by testing the socket for writing using the select function.
  This function will raise SysErr if it is called on a socket for which a previous connection
  attempt has not yet been completed.
  !*)
  val connectNB : ('af, 'sock_type) sock * 'af sock_addr -> bool
  (*!
  close sock closes the connection to the socket sock. This function raises the SysErr exception if
  the socket has already been closed.
  !*)
  val close : ('af, 'sock_type) sock -> unit
  datatype shutdown_mode = NO_RECVS | NO_SENDS | NO_RECVS_OR_SENDS
  (*!
  shutdown (sock, mode) shuts down all or part of a full-duplex connection on socket sock. If mode
  is NO_RECVS, further receives will be disallowed. If mode is NO_SENDS, further sends will be
  disallowed. If mode is NO_RECVS_OR_SENDS, further sends and receives will be disallowed. This
  function raises the SysErr exception if the socket is not connected or has been closed.
  !*)
  val shutdown : ('af, 'mode stream) sock * shutdown_mode -> unit
  (*!
  This type is an abstract name for a socket, which is used to support polling on collections of
  sockets.
  !*)
  type sock_desc
  (*!
  sockDesc sock returns a socket descriptor that names the socket sock.
  !*)
  val sockDesc : ('af, 'sock_type) sock -> sock_desc
  (*!
  sameDesc (sd1, sd2) returns true if the two socket descriptors sd1 and sd2 describe the same
  underlying socket. Thus, the expression sameDesc(sockDesc sock, sockDesc sock) will always return
  true for any socket sock.
  !*)
  val sameDesc : sock_desc * sock_desc -> bool
  (*!
  select {rds, wrs, exs, timeout} examines the sockets in rds, wrs, and exs to see if they are ready
  for reading, writing, or have an exceptional condition pending, respectively. The calling program
  is blocked until either one or more of the named sockets is ``ready '' or the specified timeout
  expires (where a timeout of NONE never expires). The result of select is a record of three lists
  of socket descriptors containing the ready sockets from the corresponding argument lists. The
  order in which socket descriptors appear in the argument lists is preserved in the result lists. A
  timeout is signified by a result of three empty lists. This function raises SysErr if any of the
  argument sockets have been closed or if the timeout value is negative. Note that one can test if a
  call to accept will block by using select to see if the socket is ready to read. Similarly, one
  can use select to test if a call to connect will block by seeing if the socket is ready to write.
  !*)
  val select : { rds : sock_desc list, wrs : sock_desc list, exs : sock_desc list, timeout : Time.time option } -> { rds : sock_desc list, wrs : sock_desc list, exs : sock_desc list }
  (*!
  ioDesc sock returns the I/O descriptor corresponding to socket sock. This descriptor can be used
  to poll the socket via pollDesc and poll in the OS.IO structure. Using the polling mechanism from
  OS.IO has the advantage that different kinds of I/O objects can be mixed, but not all systems
  support polling on sockets this way. If an application is only polling sockets, then it is more
  portable to use the select function defined above.
  !*)
  val ioDesc : ('af, 'sock_type) sock -> OS.IO.iodesc
  (*!
  Flags used in the general form of socket output operations.
  !*)
  type out_flags = {don't_route : bool, oob : bool}
  (*!
  Flags used in the general form of socket input operations.
  !*)
  type in_flags = {peek : bool, oob : bool}
  (*!
  These functions send the bytes in the slice slice on the active stream socket sock. They return
  the number of bytes actually sent. These functions raise SysErr if sock has been closed.
  !*)
  val sendVec : ('af, active stream) sock * Word8VectorSlice.slice -> int
  (*!
  See sendVec.
  !*)
  val sendArr : ('af, active stream) sock * Word8ArraySlice.slice -> int
  (*!
  These functions send the bytes in the slice slice on the active stream socket sock. They return
  the number of bytes actually sent. If the don't_route flag is true, the data is sent bypassing the
  normal routing mechanism of the protocol. If oob is true, the data is sent out-of-band, that is,
  before any other data which may have been buffered. These functions raise SysErr if sock has been
  closed.
  !*)
  val sendVec' : ('af, active stream) sock * Word8VectorSlice.slice * out_flags -> int
  (*!
  See sendVec'.
  !*)
  val sendArr' : ('af, active stream) sock * Word8ArraySlice.slice * out_flags -> int
  (*!
  These functions are the nonblocking versions of sendVec, sendVec', sendArr, and sendArr' (resp.).
  They have the same semantics as their blocking forms, with the exception that when the operation
  can complete without blocking, then the result is wrapped in SOME and if the operation would have
  to wait to send the data, then NONE is returned instead.
  !*)
  val sendVecNB : ('af, active stream) sock * Word8VectorSlice.slice -> int option
  val sendVecNB' : ('af, active stream) sock * Word8VectorSlice.slice * out_flags -> int option
  val sendArrNB : ('af, active stream) sock * Word8ArraySlice.slice -> int option
  val sendArrNB' : ('af, active stream) sock * Word8ArraySlice.slice * out_flags -> int option
  (*!
  These functions receive up to n bytes from the active stream socket sock. The size of the
  resulting vector is the number of bytes that were successfully received, which may be less than n.
  If the connection has been closed at the other end (or if n is 0), then the empty vector will be
  returned. In the second version, if peek is true, the data is received but not discarded from the
  connection. If oob is true, the data is received out-of-band, that is, before any other incoming
  data that may have been buffered. These functions raise SysErr if the socket sock has been closed
  and they raise Size if n < 0 or n > Word8Vector.maxLen.
  !*)
  val recvVec : ('af, active stream) sock * int -> Word8Vector.vector
  val recvVec' : ('af, active stream) sock * int * in_flags -> Word8Vector.vector
  (*!
  These functions read data from the socket sock into the array slice slice. They return the number
  of bytes actually received. If the connection has been closed at the other end or the slice is
  empty, then 0 is returned. For recvArr', if peek is true, the data is received but not discarded
  from the connection. If oob is true, the data is received out-of-band, that is, before any other
  incoming data that may have been buffered. These functions raise SysErr if sock has been closed.
  !*)
  val recvArr : ('af, active stream) sock * Word8ArraySlice.slice -> int
  (*!
  See recvArr.
  !*)
  val recvArr' : ('af, active stream) sock * Word8ArraySlice.slice * in_flags -> int
  (*!
  These functions are the nonblocking versions of recvVec, recvVec', recvArr, and recvArr' (resp.).
  They have the same semantics as their blocking forms, with the exception that when the operation
  can complete without blocking, then the result is wrapped in SOME and if the operation would have
  to wait for input, then NONE is returned instead.
  !*)
  val recvVecNB : ('af, active stream) sock * int -> Word8Vector.vector option
  val recvVecNB' : ('af, active stream) sock * int * in_flags -> Word8Vector.vector option
  val recvArrNB : ('af, active stream) sock * Word8ArraySlice.slice -> int option
  val recvArrNB' : ('af, active stream) sock * Word8ArraySlice.slice * in_flags -> int option
  (*!
  These functions send the message specified by the slice slice on the datagram socket sock to the
  address sa. These functions raise SysErr if sock has been closed or if the socket has been
  connected to a different address than sa.
  !*)
  val sendVecTo : ('af, dgram) sock * 'af sock_addr * Word8VectorSlice.slice -> unit
  (*!
  See sendVecTo.
  !*)
  val sendArrTo : ('af, dgram) sock * 'af sock_addr * Word8ArraySlice.slice -> unit
  (*!
  These functions send the message specified by the slice slice on the datagram socket sock to the
  address If the don't_route flag is true, the data is sent bypassing the normal routing mechanism
  of the protocol. If oob is true, the data is sent out-of-band, that is, before any other data
  which may have been buffered. These functions raise SysErr if sock has been closed or if the
  socket has been connected to a different address than sa.
  !*)
  val sendVecTo' : ('af, dgram) sock * 'af sock_addr * Word8VectorSlice.slice * out_flags -> unit
  (*!
  See sendVecTo'.
  !*)
  val sendArrTo' : ('af, dgram) sock * 'af sock_addr * Word8ArraySlice.slice * out_flags -> unit
  (*!
  These functions are the nonblocking versions of sendVecTo, sendVecTo', sendArrTo, and sendArrTo'
  (resp.). They have the same semantics as their blocking forms, with the exception that if the
  operation can complete without blocking, then the operation is performed and true is returned.
  Otherwise, false is returned and the message is not sent.
  !*)
  val sendVecToNB : ('af, dgram) sock * 'af sock_addr * Word8VectorSlice.slice -> bool
  val sendVecToNB' : ('af, dgram) sock * 'af sock_addr * Word8VectorSlice.slice * out_flags -> bool
  val sendArrToNB : ('af, dgram) sock * 'af sock_addr * Word8ArraySlice.slice -> bool
  val sendArrToNB' : ('af, dgram) sock * 'af sock_addr * Word8ArraySlice.slice * out_flags -> bool
  (*!
  These functions receive up to n bytes on the datagram socket sock, and return a pair (vec,sa),
  where the vector vec is the received message, and sa is the socket address from the which the data
  originated. If the message is larger than n, then data may be lost. In the second form, if peek is
  true, the data is received but not discarded from the connection. If oob is true, the data is
  received out-of-band, that is, before any other incoming data that may have been buffered. These
  functions raise SysErr if sock has been closed; they raise Size if n < 0 or n >
  Word8Vector.maxLen.
  !*)
  val recvVecFrom : ('af, dgram) sock * int -> Word8Vector.vector * 'sock_type sock_addr
  (*!
  See recvVecFrom.
  !*)
  val recvVecFrom' : ('af, dgram) sock * int * in_flags -> Word8Vector.vector * 'sock_type sock_addr
  (*!
  These functions read a message from the datagram socket sock into the array slice slice. If the
  message is larger than the size of the slice, then data may be lost. They return the number of
  bytes actually received. If the connection has been closed at the other end or the slice is empty,
  then 0 is returned. For recvArrFrom', if peek is true, the data is received but not discarded from
  the connection. If oob is true, the data is received out-of-band, that is, before any other
  incoming data that may have been buffered. These functions raise SysErr if sock has been closed.
  !*)
  val recvArrFrom : ('af, dgram) sock * Word8ArraySlice.slice -> int * 'af sock_addr
  (*!
  See recvArrFrom.
  !*)
  val recvArrFrom' : ('af, dgram) sock * Word8ArraySlice.slice * in_flags -> int * 'af sock_addr
  (*!
  These functions are the nonblocking versions of recvVecFrom, recvVecFrom', recvArrFrom, and
  recvArrFrom' (resp.). They have the same semantics as their blocking forms, with the exception
  that when the operation can complete without blocking, then the result is wrapped in SOME and if
  the operation would have to wait for input, then NONE is returned instead.
  !*)
  val recvVecFromNB : ('af, dgram) sock * int -> (Word8Vector.vector * 'sock_type sock_addr) option
  val recvVecFromNB' : ('af, dgram) sock * int * in_flags -> (Word8Vector.vector * 'sock_type sock_addr) option
  val recvArrFromNB : ('af, dgram) sock * Word8ArraySlice.slice -> (int * 'af sock_addr) option
  val recvArrFromNB' : ('af, dgram) sock * Word8ArraySlice.slice * in_flags -> (int * 'af sock_addr) option
end

structure Socket :> SOCKET (* OPTIONAL *) = struct end
