open Pic

module Option = struct
  let get x =
    match x with
    | None -> failwith "none"
    | Some v -> v 
end

let write_bit pin v = if v = 1 then set_bit pin else clear_bit pin
let count =
  let init = ref (Some true) in
  let pre_a = ref None in
  let count_step x =
    let a = if Option.get (!init) then 0 else (Option.get (!pre_a)) + 1 in
    init := (Some false); pre_a := (Some a); a in
  count_step
let light =
  let light_step () = let pin = if (count (0)) > 10 then 1 else 0 in pin in
  light_step
let _ =
  write_reg TRISB 0;
  while true do (write_bit RB0 (light ()); Sys.sleep 500) done
