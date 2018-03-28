module IO = struct
  let watch_inputs () =
    (true)

  let watch_outputs (h,m,s) =
    Printf.printf "%d:%d:%d\n" h m s;
    print_newline ();
    Unix.sleep 1
end

let%node count d ~return:(cpt) =
  cpt := (0 >>> cpt+1) mod d

let%node watch (s) ~return:(h,m,sec) =
  no_s = count (5 [@when s]);
  min = (no_s = 0);
  minute := count (5 [@when min]);
  m := merge min minute ((0 >>> m) [@whennot min]) ;
  ho := (minute = 0);
  hour := count (5 [@when ho]);
  h := merge ho hour ((0>>>h) [@whennot ho]);
  sec := no_s;
  (* s := no_s *)
  (* minutes_ok := (minute = 60); *)
  (* hour := count (12 [@ when minutes_ok]); *)
  (* s := seconds; *)
  (* m := merge seconds_ok minute 0; *)
  (* h := merge seconds_ok (merge minutes_ok hour 0) 0 *)
