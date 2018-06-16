module IO = struct
  let watch_inputs () = true

  let watch_outputs (h,m,s) =
    Format.printf "%02d:%02d:%02d\n%!" h m s;
    Unix.sleepf 0.001
end


let%node count d ~return:(cpt) =
  cpt = (0 --< (cpt+1) ) mod d

let%node watch (sec) ~return:(h,m,s) =
  no_s = count (60 --@ sec);
  min = (no_s = 0);
  no_m = count (60 --@ min);
  hour = (no_m = 0);
  no_h = count(12 --@ hour);
  h' = merge hour no_h ( (0 ->> h')--@ not hour);
  hh = merge min h' ((0 ->> hh) --@ not min);
  mm = merge min no_m ((0 ->> mm) --@ not min);
  h = merge sec hh (-1);
  m = merge sec mm (-1);
  s = merge sec no_s (-1)

let%node call_watch (d) ~return:(h,m,s) =
  e = true;
  (hh,mm,ss) = watch (e --@ d);
  h = merge d hh 0;
  m = merge d mm 0;
  s = merge d ss 0
