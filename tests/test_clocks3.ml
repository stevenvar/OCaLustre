
let%node three () ~return:x =
  x = 3

let%node line x ~return:y =
  y = x

let%node call_line (x,c,y,d) ~return:(f,g,w) =
  f = line x;
  g = line (10 --@ c);
  u = (g = 10);
  v = line (20 --@ u);
  w = merge u v 3
