let%node plus (a,b) ~return:(c) =
  c = a + b 

(* let%node call ~i:(a,b) ~o:(v) = *)
(*   v = plus(a,b) *)

let%node call_A (a,b) ~return:(c,d) = 
  c = plus (a,b);
  d = plus (a,b)

let%node call_B (a,b) ~return:c = 
  (c,d) = call_A(a,b)

(* let%node whn (a,c) ~return:b = *)
(*   b = a [@ when c] *)


(* let%node merge (a,k,b,c) ~return:(f,g) = *)
(*   (f,g) = ((merge c a b),(whn (k,c))) *)

(* let%node caller (u,k,v,o) ~return:(d,e) = *)
(*   (d,e) = merge (u,k,v,o) *)
