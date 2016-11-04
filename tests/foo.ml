

let%node test ~i:(x) ~o:(z) =
  pre (x >= 0); post (z >= x); inv ( a >= 0 && b >= 0 );
  a = 0 ->> (a + 1);
  b = x ->> (b + 1); 
  z = x + a + b 
