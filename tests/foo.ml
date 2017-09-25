(* let%node f ~i:(a) ~o:(x) = *)
(*   v = x + 1; *)
(*   x = a ->> v *)

(* let%node g ~i:() ~o:(u) = *)
(*   u = f 32 *)

let%node plus ~i:(a,b) ~o:(c) =
  u = 34;
  v = 56;
  j = u + v;
  k = v;
  c = a + b

(* let%node v ~i:(a,b) ~o:(c) = *)
(*   (\* (c,d) = plus(1,2); *\) *)
(*   (\* (e,f) = (4->>5,8->>9); *\) *)
(*   c = 8 ->> 9 *)

(* let%node k ~i:(a,b,c) ~o:(d,e) = *)
(*   e = a + b; *)
(*   d = (a @whn c) + b *)

let _ =
  (* INIT *)
  let s = k_init 1 2 true in
  print_int s.k_out_d;
  (* STEP *)
  for i = 0 to 10 do
    k_step s 1 2 false;
    print_int s.k_out_d;
  done
