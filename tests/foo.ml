let%node toto ~i:(k,m) ~o:(x) =
  (a,b) = (1,2);
  y = x + 1 ;
  x = k ->> y

(* let%node tata ~i:() ~o:(u) = *)
(*   (u,v) = toto (1,2)  *)
