open Pic
open Lcd
let _ = set_bit IRCF1
let _ = set_bit IRCF0
let _ = set_bit PLLEN
let disp = connect ~bus_size:Lcd.Four ~e:LATD2 ~rs:LATD3 ~rw:LATD6 ~bus:PORTB
module Option =
  struct
    let get o = match o with | None  -> failwith "None" | Some x -> x
  end
let print_drink d =
  match d with
  | 0 -> disp.print_string "."
  | 1 -> disp.print_string "C"
  | 2 -> disp.print_string "T"
let vend () =
  let vend_step (drink,cost,v) =
    let o1 = if v >= cost then drink else 0 in
    let o2 = if o1 > 0 then v - cost else v in (o1, o2) in
  vend_step
let coffee () =
  let vend_step_1 = vend () in
  let vend_step_2 = vend () in
  let init = ref (Some true) in
  let pre_money = ref None in
  let coffee_step (dime,nickel,button) =
    let (drink,v_return) =
      if button = 1
      then vend_step_1 (1, 10, (Option.get (!pre_money)))
      else
        if button = 2
        then vend_step_2 (2, 5, (Option.get (!pre_money)))
        else if button = 3 then (0, (Option.get (!pre_money))) else (0, 0) in
    let v_dime = if Option.get (!init) then 0 else if dime then 10 else 0 in
    let v_nickel = if Option.get (!init) then 0 else if nickel then 5 else 0 in
    let money =
      if Option.get (!init)
      then 0
      else
        if button > 0
        then 0
        else ((Option.get (!pre_money)) + v_dime) + v_nickel in
    init := (Some false); pre_money := (Some money); (drink, v_return, money) in
  coffee_step
let _ =
  disp.init ();
  disp.config ();
  (let main_step = coffee () in
   while true do
     let b_coffee = test_bit RD1 in
     let b_tea = test_bit RD5 in
     let b_cancel = test_bit RD4 in
     let b_dime = test_bit RB2 in
     let b_nickel = test_bit RB3 in
     let button =
       if b_coffee then 1 else if b_tea then 2 else if b_cancel then 3 else 0 in
     let (drink,return,money) = main_step (b_dime, b_nickel, button) in
     disp.clear ();
     disp.moveto 1 1;
     disp.print_string ((string_of_int money) ^ "c");
     disp.moveto 2 1;
     print_drink drink;
     disp.print_string "-->";
     disp.print_int return;
     Sys.sleep 100 done)
