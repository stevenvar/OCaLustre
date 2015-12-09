
module Option = struct
  let get o = match o with
    | None -> failwith "non"
    | Some x -> x
end 

let%node entiers (a,b) (n) = 
  n := 0 --> (pre n + 1)
  


let%node main (b1,b2) (a,b,c,d) = 
  a := 1 on b1; 
  b := 2 on b1;
  c := a + b ;
  d := entiers (b1,2)
  
let _ =
  let main_init = main () in 
  while true do
    let (a,b,c,d) = main_init (Some true, Some true) in 
    print_int (Option.get d);
    print_endline "";
    Unix.sleep 1
  done
  
