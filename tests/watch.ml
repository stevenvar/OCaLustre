module IO = struct
  let watch_inputs () = ()

  let watch_outputs (h,m,s) =
    Format.printf "%02d:%02d:%02d\n%!" h m s;
    Unix.sleepf 0.1
end

(* module IO = struct
 *
 *
 *
 *   let watch_inputs () =
 *     true
 *
 *   let watch_outputs (h,m,s) =
 *     Printf.printf "%02d:%02d:%02d\n" h m s;
 *     print_newline ();
 *     Unix.sleepf 0.01
 *
 *   let started = ref false
 *
 *   let init () =
 *     if !started = false then
 *       begin
 *         Graphics.open_graph " 200x200";
 *         Graphics.set_window_title "WATCH";
 *         Graphics.auto_synchronize false;
 *       end
 *
 *
 *   let startstop_inputs () =
 *     init ();
 *     Graphics.button_down ()
 *
 *   let startstop_outputs (h,m,s,start)=
 *     (\* watch_outputs (h,m,s); *\)
 *     let s = Printf.sprintf "%b %02d:%02d:%02d" start h m s in
 *     Graphics.clear_graph ();
 *     Graphics.moveto 100 100;
 *     Graphics.draw_string s;
 *     Graphics.synchronize ();
 *     Unix.sleepf 0.01
 *     (\* Printf.printf "start = %b \n" start *\)
 * end
 *
 * (\* let%node watch_noclock () ~return:(h,m,s) =
 *  *   s = 0 --> if (pre s) = 59 then 0 else (pre s + 1);
 *  *   m = 0 --> if s = 0 then if (pre m) = 59 then 0 else (pre m + 1) else (pre m);
 *  *   h = 0 --> if (m = 0 && s = 0) then if pre h = 11 then 0 else (pre h + 1) else pre h *\)
 *
 * let%node count d ~return:(cpt) =
 *   cpt = (0 --< (cpt+1) ) mod d
 *
 *
 * let%node watch (sec) ~return:(h,m,s) =
 *   no_s = count (60 --@ sec);
 *   min = clock (no_s = 0);
 *   no_m = count (60 --@ min);
 *   hour = clock (no_m = 0);
 *   no_h = count(12 --@ hour);
 *   h = merge min (merge hour no_h 0) ((0 --< h) --@ not min);
 *   m = merge min no_m ((0 --< m) --@ not min);
 *   s = no_s
 *
 *
 * (\* let%node watch (sec) ~return:(h,m,s) =
 *  *   no_s = count (100 --@ sec);
 *  *   min = clock (no_s = 0);
 *  *   truc = 60 --@ min;
 *  *   no_m = count (truc);
 *  *   hour = (no_m = 0);
 *  *   no_h = count (12 --@ hour);
 *  *   h' = merge hour no_h ((pre h') --@ not hour);
 *  *   h = merge min h' (pre h --@ not min);
 *  *   m = merge min no_m ((pre m)  --@ not min);
 *  *   s = no_s
 * *\)
 *
 * let%node startstop (b) ~return:(h,m,s) =
 *   (\* edge := false --> (b && (not (pre b))); *\)
 *   (\* start = true --< if edge then not start else start; *\)
 *   start = (false && b);
 *   (h,m,s) = watch (start)
 *   (\* (h',m',s') = watch (start); *\)
 *   (\* h = merge start' h' ((0 --< h) --@ not start'); *\)
 *   (\* m = merge start' m' ((0 --< m) --@ not start'); *\)
 *   (\* s = merge start' s' ((0 --< s) --@ not start') *\) *)


let%node count d ~return:(cpt) =
  cpt = (0 --< (cpt+1) ) mod d

(* let%node watch (sec) ~return:(h,m,s) =
 *   no_s = count (3 --@ sec);
 *   min = (no_s = (0 --@ sec));
 *   no_m = count ((3 --@ sec) --@ min);
 *   hour = (no_m = ((0 --@ sec) --@ min));
 *   no_h = count(((12 --@ sec) --@ min) --@ hour);
 *   h' = merge hour no_h ( (0 --@ sec --@ min --< h')--@ not hour);
 *   h = merge min h' ( (0 --@ sec --<h) --@ not min);
 *   m = merge min no_m ( (pre m) --@ not min);
 *   s = no_s *)


let%node watch (sec) ~return:(h,m,s) =
  no_s = count (3 --@ sec);
  min = (no_s = 0);
  no_m = count (3 --@ min);
  hour = (no_m = 0);
  no_h = count(3 --@ hour);
  h' = merge hour no_h ( (0 --< h')--@ not hour);
  h = merge min h' ((0 --< h) --@ not min);
  m = merge min no_m ((0 --< m) --@ not min);
  s = no_s
