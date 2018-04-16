module IO = struct
  let watch_inputs () =
    true

  let watch_outputs (h,m,s) =
    Printf.printf "%02d:%02d:%02d\n" h m s;
    print_newline ();
    Unix.sleepf 0.001
end

let%node watch_noclock () ~return:(h,m,s) =
  s = 0 --> if (pre s) = 59 then 0 else (pre s + 1);
  m = 0 --> if s = 0 then if (pre m) = 59 then 0 else (pre m + 1) else (pre m);
  h = 0 --> if (m = 0 && s = 0) then if pre h = 11 then 0 else (pre h + 1) else pre h
                                           
let%node count d ~return:(cpt) =
  cpt := (0 --< (cpt+1) ) mod d

let%node watch (sec) ~return:(h,m,s) =
  no_s = count (20 --@ sec);
  min = (no_s = 0);
  no_m = count (20 --@ min);
  hour = (no_m = 0);
  no_h = count (200 --@ hour);
  h' = merge hour no_h ((pre h') --@ not hour);
  h = merge min h' (pre h --@ not min);
  m = merge min no_m ((pre m)  --@ not min);
  s = no_s
