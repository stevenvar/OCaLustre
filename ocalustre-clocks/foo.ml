
type 'a flow = None | Nil | Value of 'a    

module Flow = struct
  type 'a t = 'a flow 

  let get o = match o with
    | None -> failwith "No value"
    | Nil -> failwith "Nil"
    | Value x -> x
end

let id x = x 

let%node main (b1,b2) (a,b,c) = 
  a := 1 on b1; 
  b := 2 on b1;
  c := a + b 

let _ =
  let main_init = main () in 
  while true do
    let (a,b,c) = main_init (Value true, Value true) in 
    print_int (Flow.get c);
    print_endline "";
    Unix.sleep 1
  done
  
