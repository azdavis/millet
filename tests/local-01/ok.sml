val _ =
  let
    local
      val x = 3
    in
      val y = x + 2
      val z = y + x
    end
  in
    y + z
  end
