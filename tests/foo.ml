let%node plus (a,b) ~return:(c) =
  c = a + b

(* let%node call ~i:(a,b) ~o:(v) = *)
(*   v = plus(a,b) *)

let%node merge (a,b,c) ~return:(f) =
  f = merge c a b
