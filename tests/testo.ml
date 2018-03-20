let%node test (x,y,c) ~return:(a,b) =
  a := x [@ when c];
  b := y [@ when_not c]

let%node test_call (c) ~return:(i,j) =
  (i,j) := test(3,8,c);
  (* k := merge c i j *)
