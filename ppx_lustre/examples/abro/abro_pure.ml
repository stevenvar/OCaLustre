open Pic
let _ = set_bit IRCF1
let _ = set_bit IRCF0
let _ = set_bit PLLEN
module Disp =
  Lcd.Connect(struct
                let bus_size = Lcd.Four
                let e = LATD0
                let rs = LATD2
                let rw = LATD1
                let bus = PORTC
              end)
module Option =
  struct
    let get o = match o with | None  -> failwith "None" | Some x -> x
  end
let moveto = Disp.moveto
let print_string = Disp.print_string
let print_bool b =
  match b with
  | true  -> Disp.print_string "t"
  | false  -> Disp.print_string "f"
let abro () =
  let init = ref (Some true) in
  let pre_seenA = ref None in
  let pre_seenB = ref None in
  let abro_step (a,b,r) =
    let seenA =
      if Option.get (!init)
      then false
      else (not r) && (a || (Option.get (!pre_seenA))) in
    let seenB =
      if Option.get (!init)
      then false
      else (not r) && (b || (Option.get (!pre_seenB))) in
    let o = seenA && seenB in
    init := (Some false);
    pre_seenA := (Some seenA);
    pre_seenB := (Some seenB);
    o in
  abro_step
let _ =
  Disp.init ();
  Disp.config ();
  write_reg TRISB 7;
  (let main_step = abro () in
   while true do
     let valA = test_bit RB0 in
     let valB = test_bit RB1 in
     let valR = test_bit RB2 in
     let o = main_step (valA, valB, valR) in
     moveto 0 1; print_string "o="; print_bool o; Sys.sleep 50 done)
