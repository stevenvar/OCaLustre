

let%node nat ~i:() ~o:y =
  y = 0 ->> (y + 1)

let%node naturels ~i:(a) ~o:(d,e,f) =
  d = a;
  e = nat ();
  f = nat ()

let _ =
  let n = naturels () in
  for i = 0 to 10 do
    let a,b,c = n (1) in
    Printf.printf "---> %d %d %d\n" a b c
  done
