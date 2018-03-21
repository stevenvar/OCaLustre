let%node test (x,y,c) ~return:(a,b) =
  a := x [@ when c];
  b := y [@ when_not c]

let%node test_call (c) ~return:(i,j) =
  (i,j) := test(3,8,c)
  (* k := merge c i j *)

let%node sample (c,y) ~return:k =
    k := y [@when c]

let%node unif (a,b,d,x) ~return:(y,p) =
  p := b && a;
  y := x [@ when p]
