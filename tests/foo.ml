let%node plus (a,b) ~return:(c) =
  c = a + b

(* let%node call ~i:(a,b) ~o:(v) = *)
(*   v = plus(a,b) *)

let%node whn (a,c) ~return:b =
  b = a [@ when c]

let%node merge (a,k,b,c) ~return:(f,g) =
  f = merge c a b;
  g = whn (k,c)

let%node caller (u,k,v,o) ~return:(d,e) =
  (d,e) = merge (u,k,v,o)
