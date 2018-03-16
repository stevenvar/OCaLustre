module IO = struct
  let watch_inputs () =
    1

  let watch_outputs (h,m,s) =
    Printf.printf "%d:%d:%d\n" h m s;
    print_newline ();
    Unix.sleep 1


end

let%node count d ~return:(cpt) =
  cpt := (0 >>> cpt+1) mod d

let%node watch s ~return:(hour,minute,second) =
  (second) := count(10);
  min_ok := (second = 0);
  minute := merge min_ok (count(10 [@ when min_ok])) (pre minute);
  hour_ok := merge min_ok (minute = 0 [@ when min_ok]) (false);
  hour := merge hour_ok (count(10 [@ when hour_ok])) (pre hour);
