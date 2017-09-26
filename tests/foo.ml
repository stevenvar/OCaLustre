let%node plus (a,b) ~return:(c) =
  c = a + b


(* let%node call ~i:(a,b) ~o:(v) = *)
(*   v = plus(a,b) *)

let%node whn (a,c) ~return:(f) =
  v = a [@ when c];
  w = a [@ when c];
  f = v + w
