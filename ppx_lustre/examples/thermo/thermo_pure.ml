open Pic
open Lcd
let output = RD1
let plus_button = RD5
let minus_button = RD4
let max_temp = 99
let def_temp = 654
let min_temp = 1010
let _ =
  set_bit IRCF1;
  set_bit IRCF0;
  set_bit PLLEN;
  clear_bit RD1;
  write_reg TRISD 253
let disp = connect ~bus_size:Lcd.Four ~e:LATD2 ~rs:LATD3 ~rw:LATD6 ~bus:PORTB
module Option =
  struct
    let get o = match o with | None  -> failwith "None" | Some x -> x
  end
let save_temp temp =
  Eeprom.write 0 (temp mod 256); Eeprom.write 1 (temp / 256); ()
let str_of_temp temp =
  if temp < max_temp
  then "++.+"
  else
    if temp > min_temp
    then "--.-"
    else Printf.sprintf "%4.1f" ((float_of_int (1033 - temp)) /. 11.67)
let load_temp () =
  let temp = (Eeprom.read 0) + ((Eeprom.read 1) * 256) in temp
let read_temp () =
  write_reg ADCON2 190;
  write_reg ADCON1 62;
  write_reg ADCON0 3;
  while test_bit GO do () done;
  ((read_reg ADRESH) lsl 8) lor (read_reg ADRES)
let buttons_state p m =
  match (p, m) with
  | (true ,false ) -> 1
  | (false ,true ) -> 2
  | (true ,true ) -> 3
  | _ -> 0
let change_wtemp () =
  let init = ref (Some true) in
  let pre_w = ref None in
  let change_wtemp_step state =
    let w =
      if Option.get (!init)
      then 654
      else
        if state = 1
        then (Option.get (!pre_w)) - 1
        else
          if state = 2
          then (Option.get (!pre_w)) + 1
          else Option.get (!pre_w) in
    init := (Some false); pre_w := (Some w); w in
  change_wtemp_step
let thermo_on () =
  let init = ref (Some true) in
  let pre_on = ref None in
  let thermo_on_step state =
    let on =
      if Option.get (!init)
      then true
      else
        if state = 3
        then not (Option.get (!pre_on))
        else Option.get (!pre_on) in
    init := (Some false); pre_on := (Some on); on in
  thermo_on_step
let heat () =
  let init = ref (Some true) in
  let pre_h = ref None in
  let heat_step (w,c) =
    let h =
      if Option.get (!init)
      then false
      else
        if c < (w - 3)
        then false
        else if c > (w + 3) then true else Option.get (!pre_h) in
    init := (Some false); pre_h := (Some h); h in
  heat_step
let save_t () =
  let init = ref (Some true) in
  let pre_w = ref None in
  let save_t_step w =
    let changed =
      if Option.get (!init) then false else w <> (Option.get (!pre_w)) in
    let save = if changed then save_temp w else () in
    init := (Some false); pre_w := (Some w); save in
  save_t_step
let main () =
  let change_wtemp_step_2 = change_wtemp () in
  let heat_step_3 = heat () in
  let save_t_step_4 = save_t () in
  let thermo_on_step_1 = thermo_on () in
  let init = ref (Some true) in
  let main_step (plus,minus,ctemp) =
    let state = buttons_state plus minus in
    let on = thermo_on_step_1 state in
    let wtemp =
      if Option.get (!init)
      then load_temp ()
      else if on then change_wtemp_step_2 state else 0 in
    let heat = if on then heat_step_3 (wtemp, ctemp) else false in
    let save = save_t_step_4 wtemp in init := (Some false); (wtemp, on, heat) in
  main_step
let _ =
  disp.init ();
  disp.config ();
  disp.register_bitmap '\000' 458 14336 0;
  disp.register_bitmap '\001' 4 8840 4096;
  disp.register_bitmap '\002' 4 2210 4096;
  disp.register_bitmap '\003' 16 8840 16384;
  disp.register_bitmap '\004' 1 2210 1024;
  disp.print_string "\00132.5\000C\002\n 32.5\000C ";
  (let main_step = main () in
   while true do
     let plus = test_bit plus_button in
     let minus = test_bit minus_button in
     let t = read_temp () in
     let (wtemp,on,heat) = main_step (plus, minus, t) in
     if on
     then
       (disp.moveto 1 1;
        disp.print_string (str_of_temp wtemp);
        disp.moveto 2 1;
        disp.print_string (str_of_temp t);
        if heat then set_bit output else clear_bit output);
     Sys.sleep 300 done)
