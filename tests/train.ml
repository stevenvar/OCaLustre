

let%node counter (init,incr,x,reset) ~return:(c) =
  pc := init --> (0 fby c);
  c := if reset then init else if x then (pc + incr) else pc

let%node speed (beacon,second) ~return:(late,early,ok1,ok2) =
  incr := if beacon && not second then 1
    else if second && not beacon then -1
    else 0 ;
  diff := counter(0,incr,  beacon || second, false);
  early := false --> (if pre early then (diff > 0)
                     else diff >= 10);
  late := false --> (if pre late then (diff < 0)
                     else diff <= -10);
  ok1 := not (early && late);
  ok2 := true --> not (late && pre early)
    

let _ =
  let b_of_i = function 0 -> false | _ -> true in 
  let speed = speed () in
  for i = 0 to 100 do
    let b = read_int () |> b_of_i in 
    let s = read_int () |> b_of_i in 
    let late,early,ok1,ok2 = speed (b,s) in
    Printf.printf "late = %b, early = %b, ok1 = %b, ok2 = %b \n" late early ok1 ok2
  done
