fun 'a foo (x: 'a) =
  let
    exception Poly of 'a
  in
    raise Poly x; raise Poly 3; ()
  end
