
module Option = struct
  let get o = match o with
    | None -> failwith "non"
    | Some x -> x
end 


let%node entiers (a,b,c) (n) =
  n := (a+b) --> ( (pre n) + 1 )  

let%node main () (n) =
  h:= true --> false; 
  n := entiers (1,2, true)

let _ =
  let entiers_step = main () in 
  while true do
    let n = entiers_step () in 
    print_int n; 
    print_endline "";
    Unix.sleep 1
  done
  
