

let%node naturels ~inf:() ~outf:(d) =
  d = 0 --> (pre d + 1)

let _ =
    let nat_step = naturels () in  
  for i = 0 to 10 do
    print_int (nat_step ())
  done; 
  
