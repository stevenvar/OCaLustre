


let%node lolol ~i:() ~o:x =
  x = 1 ->> (2 ->> (3 ->> (4 ->> x)))

let%node fibo ~i:() ~o:(g,f) =
  l = 1 ->> g;
  k = g + l;
  g = 0 ->> k;
  f = 0 ->> ( 1 --> (pre f + f ))

let%node test ~i:(i,j) ~o:(c,o) =
  c = true --> (not (pre c));
  o = i @ c

let _ =
  let n = fibo () in
  for i = 0 to 30 do
    let a,b= n () in
    Printf.printf "---> %d \n" a
  done
