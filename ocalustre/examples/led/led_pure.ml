open Pic
module Disp =
  Lcd.Connect(struct
                let bus_size = Lcd.Four
                let e = RC5
                let rs = RD3
                let rw = RC4
                let bus = PORTB
              end)
module Option =
  struct
    let get o = match o with | None  -> failwith "None" | Some x -> x
  end
let moveto = Disp.moveto
let print_string = Disp.print_string
let cpt () =
  let init = ref (Some true) in
  let pre_n = ref None in
  let cpt_step () =
    let n = (if Option.get (!init) then 0 else Option.get (!pre_n)) + 1 in
    init := (Some false); pre_n := (Some n); n in
  cpt_step
let main =
  let cpt_step_1 = cpt () in 
  let cpt_step_2 = cpt () in 
  let main_step () = let a = cpt_step_1 () in let b = cpt_step_2 () in (a, b) in main_step
let _ =
  Disp.init ();
  Disp.config ();
  while true do
    (let (a,b) = main () in
     Disp.moveto 1 0;
     Disp.print_int a;
     Disp.moveto 1 4;
     Disp.print_int b;
     Sys.sleep 1000)
    done
