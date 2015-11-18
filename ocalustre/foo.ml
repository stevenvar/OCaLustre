
module Option = struct
  let get o = match o with
    | None -> failwith "non"
    | Some x -> x
end 


let%node entiers () (n) =
  n := 0.0 --> ( (pre n) +. 1.0)  
  

let%node main () (a,b) = 
  a := entiers (); 
  b := entiers ()
  
let _ =
  let main_init = main () in 
  while true do
    let (a,b) = main_init () in 
    print_float a;
    print_float b; 
    print_endline "";
    Unix.sleep 1
  done
  
