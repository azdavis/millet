val _ = fn f => fn x => (
  f x x;
  f x;
  f x x x andalso false;
  f 3;
  f: unit;
  false
)
