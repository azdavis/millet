val id = fn x => x
val _ = id 3
val _ = id "hey"
val _ = if id false then id 1 + 1 else id (2 + 2)
