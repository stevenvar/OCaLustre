module IO = struct
  let call_watch_inputs () =
    ()

  let call_watch_outputs (h,m,s) =
    Printf.printf "%02d:%02d:%02d\n" h m s;
    print_newline ();
    Unix.sleepf 0.001
end

let%node count d ~return:(cpt) =
  cpt := (0 >>> cpt+1) mod d

let%node watch (sec) ~return:(hour,min,h,m,s) =
  no_s = count (60 [@when sec]);
  min = (no_s = 0);
  no_m = count (60 [@when min]);
  hour = (no_m = 0);
  no_h = count (12 [@when hour]);
  (* h = merge min (merge hour no_h 0) ((0 >>> h) [@whennot min]); *)
  (* m = merge min no_m ((0 >>> m) [@whennot min]); *)
  h = no_h;
  m = no_m;
  s = no_s

let%node call_watch () ~return:(h,m,s) =
  sc = true;
  (hr,mn,h1,m1,s1) = watch (sc);
  (* h' = (merge hour h1 ( (0 >>> h') [@whennot hour])); *)
  (* h = merge min h' ((0 >>> h) [@whennot min]); *)
  (* m = merge min m1 ((0 >>> m)[@whennot min]); *)
  h'' = merge hr h1 (0 fby h'' [@whennot hr]);
  h' = merge mn (h'') (0 fby h' [@whennot mn]);
  m' = merge mn m1 ((0 fby m') [@whennot mn]);
  s' = s1;
  h = merge sc h' 9;
  m = merge sc m' 9;
  s = merge sc s' 9
