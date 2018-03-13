open Pic
open Lcd
type kind =
  | Nothing 
  | Plus 
  | Minus 
  | Twice 
[@@@ocaml.text " CONFIG *"]
let output = RD1 
let plus_button = RD5 
let minus_button = RD4 
let max_temp = 99 
let def_temp = 654 
let min_temp = 1010 
let config =
  set_bit IRCF1;
  set_bit IRCF0;
  set_bit PLLEN;
  write_reg TRISD 0b11111101;
  clear_bit RD1 
let disp = connect ~bus_size:Lcd.Four ~e:LATD2 ~rs:LATD3 ~rw:LATD6 ~bus:PORTB 
let disp_inits =
  disp.init ();
  disp.config ();
  disp.register_bitmap '\000' 0b01110_01010 0b01110_00000_00000
    0b00000_00000_00000;
  disp.register_bitmap '\001' 0b00000_00100 0b01000_10100_01000
    0b00100_00000_00000;
  disp.register_bitmap '\002' 0b00000_00100 0b00010_00101_00010
    0b00100_00000_00000;
  disp.register_bitmap '\003' 0b00000_10000 0b01000_10100_01000
    0b10000_00000_00000;
  disp.register_bitmap '\004' 0b00000_00001 0b00010_00101_00010
    0b00001_00000_00000;
  disp.print_string "\00132.5\000C\002\n 32.5\000C " 
let str_of_temp temp =
  if temp < max_temp
  then "++.+"
  else
    if temp > min_temp
    then "--.-"
    else
      (let f = (float_of_int (1033 - temp)) /. 11.67  in
       Printf.sprintf "%4.1f" f)
  
let save_temp temp =
  Eeprom.write 0 (temp mod 256); Eeprom.write 1 (temp / 256); () 
let load_temp () =
  let temp = (Eeprom.read 0) + ((Eeprom.read 1) * 256)  in
  if (temp < max_temp) || (temp > min_temp) then def_temp else temp 
let read_temp () =
  write_reg ADCON2 0b10111110;
  write_reg ADCON1 0b00111110;
  write_reg ADCON0 0b00000011;
  while test_bit GO do () done;
  ((read_reg ADRESH) lsl 8) lor (read_reg ADRES) 
let counter = ref 0 
let kind = ref Nothing 
let buttons_state p m =
  match (p, m, (!kind), (!counter)) with
  | (true ,false ,Nothing ,0) -> (kind := Plus; incr counter; 1)
  | (true ,false ,Plus ,10) -> 10
  | (true ,false ,Plus ,_) -> (incr counter; 1)
  | (false ,true ,Nothing ,0) -> (kind := Minus; incr counter; 2)
  | (false ,true ,Minus ,10) -> 20
  | (false ,true ,Minus ,_) -> (incr counter; 2)
  | (true ,true ,_,_) -> (counter := 1; kind := Twice; 3)
  | _ -> (kind := Nothing; counter := 0; 0) 
let update_prop (wtemp,ctemp) =
  let st_new_prop = ref 0  in
  let update_prop_step (wtemp,ctemp) =
    let new_prop = !st_new_prop  in
    let delta = min 10 (max (-10) (ctemp - wtemp))  in
    let offset =
      min 10 (if delta < 0 then (- delta) * delta else delta * delta)  in
    let prop = new_prop / 10  in
    st_new_prop := (min 100 (max 0 ((!st_new_prop) + offset))); prop  in
  update_prop_step 
let timer number =
  let st_time = ref 1  in
  let timer_step number =
    let time = !st_time  in
    let alarm = time < number  in
    st_time := (if time = 10 then 1 else time + 1); alarm  in
  timer_step

let heat (w,c) =
  let update_prop1_step = update_prop (w, c)  in
  let prop = update_prop1_step (w,c) in 
  let timer2_step = timer prop  in
  let heat_step (w,c) =
    let prop = update_prop1_step (w, c)  in let h = timer2_step prop  in h
     in
     heat_step

let change_wtemp (default,state) =
  let st_w = ref default  in
  let change_wtemp_step (default,state) =
    let w = !st_w  in
    st_w :=
      (if state = 1
       then w - 1
       else
         if state = 10
         then w - 3
         else if state = 2 then w + 1 else if state = 20 then w + 3 else w);
    w  in
  change_wtemp_step 
let thermo_on state =
  let st_on = ref true  in
  let thermo_on_step state =
    let on = !st_on  in st_on := (if state = 3 then not on else on); on  in
  thermo_on_step 
let save_t w =
  let st_changed = ref false  in
  let st_pre_w = ref 0  in
  let save_t_step w =
    let pre_w = !st_pre_w  in
    let changed = !st_changed  in
    let save = if changed then () else ()  in
    st_pre_w := w; st_changed := (w <> pre_w); save  in
  save_t_step 
let main (plus,minus,ctemp) =
  let state = buttons_state plus minus  in
  let change_wtemp1_step = change_wtemp ((load_temp ()), state)  in
  let thermo_on2_step = thermo_on state  in
  let wtemp = if on then _aux_1 else 0  in
  let save_t4_step = save_t wtemp  in
  let heat3_step = heat (wtemp, ctemp)  in
  let main_step (plus,minus,ctemp) =
    let state = buttons_state plus minus  in
    let _aux_1 = change_wtemp1_step ((load_temp ()), state)  in
    let on = thermo_on2_step state  in
    let wtemp = if on then _aux_1 else 0  in
    let _aux_2 = heat3_step (wtemp, ctemp)  in
    let h = if on then _aux_2 else false  in
    let save = save_t4_step wtemp  in (wtemp, on, h)  in
  main_step 
let _ =
  set_bit IRCF1;
  set_bit IRCF0;
  set_bit PLLEN;
  clear_bit RD1;
  write_reg TRISD 0b11111101;
  disp.init ();
  disp.config ();
  disp.register_bitmap '\000' 0b01110_01010 0b01110_00000_00000
    0b00000_00000_00000;
  disp.register_bitmap '\001' 0b00000_00100 0b01000_10100_01000
    0b00100_00000_00000;
  disp.register_bitmap '\002' 0b00000_00100 0b00010_00101_00010
    0b00100_00000_00000;
  disp.register_bitmap '\003' 0b00000_10000 0b01000_10100_01000
    0b10000_00000_00000;
  disp.register_bitmap '\004' 0b00000_00001 0b00010_00101_00010
    0b00001_00000_00000;
  disp.print_string "\00132.5\000C\002\n 32.5\000C ";
  (let main_step = main (false, false, 0)  in
   while true do
     let plus = test_bit plus_button  in
     let minus = test_bit minus_button  in
     let t = read_temp ()  in
     let (wtemp,on,heat) = main_step (plus, minus, t)  in
     if on
     then
       (disp.moveto 1 1;
        disp.print_string (str_of_temp wtemp);
        disp.moveto 2 1;
        disp.print_string (str_of_temp t);
        if heat then set_bit output else clear_bit output);
     Sys.sleep 100 done)



