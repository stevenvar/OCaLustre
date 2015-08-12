module Option = struct
  let get x =
    match x with
    | None -> failwith "none"
    | Some v -> v 
  end

let%node xor () (a,b) =
  b:= if a < 10 then true else false ;
  a:= 0 next (pre a + 1)
  
let _ =
  while true do
    let (a,b) = xor () in
    Format.printf "%d | %B @." a b;
    Unix.sleep 1
  done
