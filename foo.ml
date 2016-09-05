


let%node id ~i:(x,f) ~o:x =
  x = x 

let%node fibo ~i:() ~o:(g,f) =
  l = 1 ->> g;
  k = g + l;
  g = 0 ->> k;
  f = 0 ->> ( 1 --> (pre f + f ))

let%node test ~i:(x,c) ~o:(y,ck) =
  y = id (x,c);
  ck = c

let%node foo ~i:(x,c) ~o:y =
  y = test (x,c)


let _ =
  let n = fibo () in
  for i = 0 to 30 do
    let a,b= n () in
    Printf.printf "---> %d \n" a
  done
