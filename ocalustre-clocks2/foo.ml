


let%node naturels ~i:() ~o:(d) =
  d = 1 fby (1 --> (pre d + d ))


let _ =
  let nat = naturels () in
  for i = 0 to 10 do
    Printf.printf "%d\n" (nat ())
  done
