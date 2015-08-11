module Option = struct
  let get x =
    match x with
    | None -> failwith "none"
    | Some v -> v 
  end

let%node add (a,b) (c) =
  c := a + b 
 
let%node xor (b,c) (a) =
 a := add (b,c) 

let _ =
  while true do
    Format.printf "%d @." (xor (4,5));
    Unix.sleep 1
  done
