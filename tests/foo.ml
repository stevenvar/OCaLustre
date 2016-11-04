    

let%node test ~i:(x) ~o:(z) =
  pre (x >= 0); post (z >= x); inv (y >= 0 && w >= b);
  y = 0 ->> (y + 1);
  w = x ->> (w + 1);
  z = x + y
