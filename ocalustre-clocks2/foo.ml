
let%node lol ~inf:a ~outf:b =
  b = a 

let%node naturels ~inf:() ~outf:(d) =
  d = 0 --> (pre d + 1)

let _ =
  let nat_step = naturels () in
  let lol_step = lol () in 
  for i = 0 to 10 do
    print_int (nat_step ());
    print_int (lol_step (i*2))
  done; 
  
