let naturels () =
let pre_d = ref 0 in
 
let naturels_step () = 
let d = !pre_d in 
pre_d := !pre_d + 1;
d
in
naturels_step

let _ =
  let nat_step = naturels () in
  for i = 0 to 10 do
    print_int (nat_step ()); 
  done 
