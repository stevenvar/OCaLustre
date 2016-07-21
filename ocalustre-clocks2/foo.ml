


let%node naturels ~i:(i) ~o:(d) =
  d = 0 --> (pre d + 1) * i 


let%node main ~i:() ~o:(x,y) =
  x = (naturels (2));
  y = (naturels (1)) 


let _ =
  let main_step = main () in 
  for i = 0 to 10 do
    let (x,y) = main_step () in 
    Format.printf "%d %d \n" x y 
  done 
