
open Pic
open Lcd
type kind = Nothing | Plus | Minus | Twice

type state = SlowPlus | FastPlus | SlowMinus | FastMinus | OnOff | Default 

let output = RD1
let plus_button = RD5
let minus_button = RD4

let max_temp =   99 (* 80.0°C *)
let def_temp =  654 (* 32.5°C *)
let min_temp = 1010 (* 2°C *)

let _ = 
  set_bit IRCF1;
  set_bit IRCF0;	
  set_bit PLLEN;
  clear_bit RD1;
  write_reg TRISD 0b11111101

let disp = connect ~bus_size:Lcd.Four ~e:LATD2 ~rs:LATD3 ~rw:LATD6 ~bus:PORTB;;

let disp_inits =
  disp.init ();
  disp.config ();
  disp.register_bitmap '\000'
    0b01110_01010 0b01110_00000_00000 0b00000_00000_00000;
  disp.register_bitmap '\001'
    0b00000_00100 0b01000_10100_01000 0b00100_00000_00000;
  disp.register_bitmap '\002'
    0b00000_00100 0b00010_00101_00010 0b00100_00000_00000;
  disp.register_bitmap '\003'
    0b00000_10000 0b01000_10100_01000 0b10000_00000_00000;
  disp.register_bitmap '\004'
    0b00000_00001 0b00010_00101_00010 0b00001_00000_00000;
  disp.print_string "\00132.5\000C\002\n 32.5\000C "


let save_temp temp =
  Eeprom.write 0 (temp mod 256);
  Eeprom.write 1 (temp / 256);
  ()

let str_of_temp temp =
  if temp < max_temp then "++.+"
  else if temp > min_temp then "--.-"
  else
   (* less memory hungry : let v = (float_of_int (1033 - temp) /. 11.67) in
    let s = string_of_float v in 
      String.sub s 0 4 *)
     Printf.sprintf "%4.1f" (float_of_int (1033 - temp) /. 11.67) 

let load_temp () =
  let temp = Eeprom.read 0 + Eeprom.read 1 * 256 in
  if temp < max_temp || temp > min_temp then def_temp else
    temp 

let read_temp () =
  write_reg ADCON2 0b10111110;
  write_reg ADCON1 0b00111110;
  write_reg ADCON0 0b00000011;
  while test_bit GO do () done;
  (read_reg ADRESH lsl 8) lor read_reg ADRES


let print_temps (wtemp,temp) =
   disp.moveto 1 1;
   disp.print_string (str_of_temp wtemp);
   disp.moveto 2 1;
   disp.print_string (str_of_temp temp)
     
let counter = ref 0 
let kind = ref Nothing 
let buttons_state p m = 
  match p , m, !kind, !counter with 
  | true , false, Nothing , 0 -> kind := Plus; incr counter; SlowPlus
  | true , false, Plus , 10 -> FastPlus
  | true , false, Plus , _ -> incr counter; SlowPlus
  | false , true, Nothing, 0 -> kind := Minus; incr counter; SlowMinus
  | false , true, Minus, 10 -> FastMinus
  | false , true, Minus, _ -> incr counter; SlowMinus
  | true , true, _, _ -> counter := 1; kind := Twice; OnOff
  | _ -> kind := Nothing; counter := 0 ; Default


let%node update_prop (wtemp,ctemp) ~return:(prop) =
  new_prop := 0 >>> ( eval (min 100 (max 0 (new_prop + offset)))); 
  delta := eval (min 10 (max (-10) (ctemp-wtemp)));
  offset := eval (min 10 (if delta < 0 then ( (-delta) * delta) else (delta * delta)) ); 
  prop := new_prop / 10

let%node timer (number) ~return:(alarm) =
  time := 1 >>> if (time) = 10 then 1 else (time) + 1;
  alarm := (time < number) 

let%node heat (w,c) ~return:(h) =
  prop := update_prop (w,c);
  h := timer (prop)

let%node change_wtemp (default,state) ~return:(w) = 
  w := default --> (0 >>>
      if state = SlowPlus then w - 1 
      else if state = FastPlus then w - 3 
      else if state = SlowMinus then w + 1
      else if state = FastMinus then w + 3 
      else w ) 

let%node thermo_on (state) ~return:(on) = 
  on := true >>> (if state = OnOff then not (on) else (on))

let%node save_t (w) ~return:(save) =
  pre_w := 0 >>> w; 
  changed := false >>> (w <> (pre_w));
  save := eval (save_temp w) [@ when changed]

let%node thermo (plus,minus,ctemp) ~return:(wtemp, on, h) =
  state := eval ( buttons_state plus minus );
  on := thermo_on (state); 
  wtemp := change_wtemp ( (eval (load_temp ()) ),state) [@when on]; 
  h := heat (wtemp, ctemp) [@ when on];
  save := save_t wtemp

let get_plus () = test_bit plus_button
let get_minus () = test_bit minus_button
let get_t () = read_temp ()

let out_wt wt = print_temps (get_t (), wt)
let out_heat h = if h then set_bit output else clear_bit output

let get_inputs () = (get_plus (), get_minus (), get_t ())
let write_outputs (wt,h) = (out_wt wt; out_heat h)

let%node main (plus,minus,t) ~return:(wanted_temp,heat) =
  (wanted_temp,on,heat) := thermo (plus,minus,t); 

  
  
  

