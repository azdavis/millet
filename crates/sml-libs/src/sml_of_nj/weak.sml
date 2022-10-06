(*!
A weak pointer is one that is not sufficient to keep an object live. If some object x is pointed to
by ordinary pointers and by weak pointers, it will stay live (not be garbage collected); but if it
is pointed to only by weak pointers, then it may be garbage collected.

As an example, suppose we want to make a "finalization" function for some data structure t. We can
create a w=weak(t) and put w onto a list (of things we want to finalize). Then we can watch this
list from time to time, and when strong(w)=NONE we can perform the finalization procedure. A good
time to examine the list is upon receipt of the GC signal; see Signals.

The semantics of weak pointers to immutable data structures in ML is ambiguous. For example,
consider the following:

Finally, in this example:

One way of avoiding these problems, this year, is to apply weak only to ref objects, which have a
stronger notion of object identity than do ordinary records, tuples, and data constructors. This
works as long as the compiler does not optimize refs too much, which at present is a safe
assumption.
!*)
signature WEAK = sig
  (*!
  weak a Make a weak pointer to object a.
  !*)
  type 'a weak
  (*!
  weak a Make a weak pointer to object a.
  !*)
  val weak : 'a -> 'a weak
  (*!
  strong w The result is NONE if the object to which w points has already been garbage-collected; or
  SOME(a) if w points to some still-live object a.
  !*)
  val strong : 'a weak -> 'a option
  (*!
  weak' a Make an opaque weak pointer to a.
  !*)
  type weak'
  (*!
  weak' a Make an opaque weak pointer to a.
  !*)
  val weak' : 'a -> weak'
  (*!
  strong' w Tell if the underlying object is still alive.
  !*)
  val strong' : weak' -> bool
end

structure Weak : WEAK = struct end
