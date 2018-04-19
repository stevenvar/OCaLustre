
let%node trois () ~return:x =
  x = 3

let%node fil x ~return:y =
  y = x

let%node test (c,d) ~return:(k) =
  y = 2 --@ c;
  z = 3 --@ d;
  k = y + z

let%node merge (c,x,y,u,v) ~return:(m,n) =
  m = merge c x y;
  n = merge c (u --@ c) (v --@ not c)

(* let%node wrong () ~return:(c,x) = *)
  (* c = clock true; *)
  (* x = 2 --@ c *)
