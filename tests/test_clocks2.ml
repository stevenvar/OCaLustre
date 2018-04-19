let%node merger (x,y,c) ~return:z =
  z = merge c (y --@ c) (x --@ not c)

let%node trois x ~return:t = (t = x + 3)

let%node call_trois1 (a,e) ~return:t =
  t = trois (a [@when e])

let%node call_trois2 (a,c,d) ~return:(t,v) =
  u = a --@ d;
  v = c --@ d;
  t = (u,v)  (* call_trois1 (u,v) *)

let%node call_trois3 (a,b,c,d) ~return:(t,k,w) =
  w = clock (c && d);
  u = a --@ w;
  v = b --@ w;
  (t,k) = (u, v)

  (* call_trois2 (u,v,w) *)

(* let%node call_trois (a,c,d) ~return:(t,v) =
 *   t = trois (a [@when c]);
 *   v = trois (a [@when d]) *)
