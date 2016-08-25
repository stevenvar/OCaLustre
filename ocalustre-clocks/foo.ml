

let%node naturels ~i:() ~o:(n,v) =
  x = 4. ;
  v = -. x;
  n = 1 --> (pre (1 --> ( (pre n) + n)))

let _ =
  let nat = naturels () in
  for i = 0 to 10 do
    let d,v = nat () in
    Printf.printf "---> %d %f\n" d v ;
  done
