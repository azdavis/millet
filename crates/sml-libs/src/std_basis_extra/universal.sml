signature UNIVERSAL = sig
  type universal
  type 'a tag
  val tag : unit -> 'a tag
  val tagInject : 'a tag -> 'a -> universal
  val tagIs : 'a tag -> universal -> bool
  val tagProject : 'a tag -> universal -> 'a
end

structure Universal :> UNIVERSAL = struct end
