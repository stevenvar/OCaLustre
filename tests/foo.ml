let%node toto ~i:(k,m) ~o:(x) =
  x = k ->> m

let%node tata ~i:() ~o:(u,v) =
  u = toto (1,2);
  v = 3
