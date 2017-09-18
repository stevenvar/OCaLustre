(* let%node f ~i:(a) ~o:(x) = *)
(*   v = x + 1; *)
(*   x = a ->> v *)

(* let%node g ~i:() ~o:(u) = *)
(*   u = f 32 *)

let%node plus ~i:(a,b) ~o:(c,d) =
  c = a + b;
  d = a - b

let%node v ~i:(a,b) ~o:(c,d) =
  (c,d) = ekekek(1,2)

let _ =
  let s = v_init 1 2  in
  print_int s.v_out_c;
  for i = 0 to 10 do
    v_step s 1 2;
    print_int s.v_out_c;
  done
