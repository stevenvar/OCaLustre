(* let%node test (x,y,c) ~return:(a,b) = *)
  (* a := x [@ when c]; *)
  (* b := y [@ when_not c] *)

(* let%node test_call (x,y,c) ~return:(i,j) = *)
  (* (i,j) := test(x,y,c) *)
  (* k := merge c i j *)

let%node sample (c,y) ~return:k =
  k := y [@when c]


let%node call_sample (c,d,y) ~return:(v,k) =
  v := c [@when d];
    k := sample(v,y)

let%node unif (a,b,d,x) ~return:(y,p) =
  p := b && a;
  y := x [@ when p]

let%node call_unif (a,b,d,x) ~return:(y,p) =
  (y,p) = unif (a,b,d,x)
