(* let%node f ~i:(a) ~o:(x) = *)
(*   v = x + 1; *)
(*   x = a ->> v *)

(* let%node g ~i:() ~o:(u) = *)
(*   u = f 32 *)

let%node plus ~i:(a,b) ~o:(y,z) =
  y = (a+b);
  z = (a-b)

let%node v ~i:(x,y,z) ~o:(k,l,m,r) =
  (k,r) = plus (x,x);
  l = y;
  m = z

let _ =
  let s = v_0 1 2.5 "OU" in
  for i = 0 to 10 do
    v_next s 1 2.5 "OU";
    print_int s.v_out_k;
    print_int s.v_out_r;
  done
