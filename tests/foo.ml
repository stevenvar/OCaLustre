(* let%node f ~i:(a) ~o:(x) = *)
(*   v = x + 1; *)
(*   x = a ->> v *)

(* let%node g ~i:() ~o:(u) = *)
(*   u = f 32 *)

let%node plus ~i:(a,b) ~o:(c,d) =
  c = a + b;
  d = a - b

let%node v ~i:(a,b) ~o:(c) =
  (* (c,d) = plus(1,2); *)
  (* (e,f) = (4->>5,8->>9); *)
  c = 8 ->> 9

let%node k ~i:(a,c) ~o:(b) =
  f = a @whn c;
  d = a ;
  b = f + d

let _ =
  (* INIT *)
  let s = k_init 1 true in
  print_int s.k_out_b;
  (* STEP *)
  for i = 0 to 10 do
    k_step s 1 false;
    print_int s.k_out_b;
  done
