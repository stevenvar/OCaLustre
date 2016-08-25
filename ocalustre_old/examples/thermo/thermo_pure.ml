open Pic
open Lcd
let max_temp = 99
let def_temp = 654
let min_temp = 1010
let output = RD1
let plus_button = RD5
let minus_button = RD4
let _ = set_bit IRCF1
let _ = set_bit IRCF0
let _ = set_bit PLLEN
let _ = clear_bit RD1
let _ = write_reg TRISD 253
let disp = connect ~bus_size:Lcd.Four ~e:LATD2 ~rs:LATD3 ~rw:LATD6 ~bus:PORTB
let _ = disp.init ()
let _ = disp.config ()
let _ = disp.register_bitmap '\000' 458 14336 0
let _ = disp.register_bitmap '\001' 4 8840 4096
let _ = disp.register_bitmap '\002' 4 2210 4096
let _ = disp.register_bitmap '\003' 16 8840 16384
let _ = disp.register_bitmap '\004' 1 2210 1024
let _ = disp.print_string "\00132.5\000C\002\n 32.5\000C "
module Option =
  struct
    let get o = match o with | None  -> failwith "None" | Some x -> x
  end
let str_of_temp temp =
  if temp < max_temp
  then "++.+"
  else
    if temp > min_temp
    then "--.-"
    else Printf.sprintf "%4.1f" ((float_of_int (1033 - temp)) /. 11.67)
let read_temp () =
  write_reg ADCON2 190;
  write_reg ADCON1 62;
  write_reg ADCON0 3;
  while test_bit GO do () done;
  ((read_reg ADRESH) lsl 8) lor (read_reg ADRES)
let mode () =
  let init = ref (Some true) in
  let pre_m = ref None in
  let mode_step (plus,minus) =
    let m =
      if Option.get (!init)
      then 0
      else
        if plus && (not minus)
        then 1
        else
          if (not plus) && minus
          then 2
          else if plus && minus then 3 else Option.get (!pre_m) in
    init := (Some false); pre_m := (Some m); m in
  mode_step
let thermo () =
  let mode_step_1 = mode () in
  let init = ref (Some true) in
  let pre_heat = ref None in
  let pre_temp1 = ref None in
  let thermo_step (plus,minus,def,t) =
    let mode = mode_step_1 (plus, minus) in
    let temp2 = t in
    let value_moins =
      if Option.get (!init)
      then def
      else
        if mode = 2
        then (Option.get (!pre_temp1)) + 3
        else (Option.get (!pre_temp1)) + 1 in
    let value_plus =
      if Option.get (!init)
      then def
      else
        if mode = 1
        then (Option.get (!pre_temp1)) - 3
        else (Option.get (!pre_temp1)) - 1 in
    let new_temp1 =
      if Option.get (!init)
      then def
      else
        if plus
        then value_plus
        else if minus then value_moins else Option.get (!pre_temp1) in
    let temp1 = if Option.get (!init) then def else new_temp1 in
    let heat =
      if Option.get (!init)
      then true
      else
        if t < (temp1 - 3)
        then false
        else if t > (temp1 + 3) then true else Option.get (!pre_heat) in
    init := (Some false);
    pre_heat := (Some heat);
    pre_temp1 := (Some temp1);
    (temp1, temp2, heat) in
  thermo_step
let _ =
  let main_step = thermo () in
  while true do
    let plus = test_bit plus_button in
    let minus = test_bit minus_button in
    let t = read_temp () in
    let (temp1,temp2,heat) = main_step (plus, minus, def_temp, t) in
    disp.moveto 1 1;
    disp.print_string (str_of_temp temp1);
    disp.moveto 2 1;
    disp.print_string (str_of_temp temp2);
    if heat then set_bit output else clear_bit output;
    Sys.sleep 300 done
