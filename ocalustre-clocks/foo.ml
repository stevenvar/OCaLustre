
module Option = struct
  let get o = match o with
    | None -> failwith "non"
    | Some x -> x
end 



let%node main (b1,b2) (a,b,c) = 
  a := 1 on b1; 
  b := 2 on b1;
  c := a + b 
  
let _ =
  let main_init = main () in 
  while true do
    let (a,b) = main_init (true,true) in 
    print_int a;
    print_int b; 
    print_endline "";
    Unix.sleep 1
  done
  
