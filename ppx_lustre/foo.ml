module Option = struct
  let get x =
    match x with
    | None -> failwith "none"
    | Some v -> v 
  end

let%node xor () (a) =
  a := 0 next (pre a + 1)

let _ =
    while true do
    let a = xor () in
    Format.printf "%d @." a;
    Unix.sleep 1
    done
