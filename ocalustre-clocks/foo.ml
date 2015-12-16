
module Option = struct
  type 'a t = 'a option

  let get o = match o with
    | None -> failwith "No value"
    | Some x -> x
end

let%node truc (a,h) (z) =
  y := a on h;
  z := current y


let _ = 
  let main_step = truc () in
  let cpt = ref 1 in 
  while true do 
    let z = main_step (!cpt,true) in
    print_int z;
    print_endline "";
    Unix.sleep 1;
    incr cpt
  done 
