module Option =
  struct
    let get x = match x with | None  -> failwith "none" | Some v -> v
  end
let xor =
  let init = ref (Some true) in
  let pre_a = ref None in
  let xor_step () =
    let a =
      ((if Option.get (!init) then 0 else (Option.get (!pre_a)) + 1), true) in
    init := (Some false); pre_a := (Some a); a in
  xor_step
let _ =
  while true do let (a,b) = xor () in Format.printf "%d @." a; Unix.sleep 1
    done
