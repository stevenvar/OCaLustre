module IO = struct



  let watch_inputs () =
    true

  let watch_outputs (h,m,s) =
    Printf.printf "%02d:%02d:%02d\n" h m s;
    print_newline ();
    Unix.sleepf 0.01

  let started = ref false

  let init () =
    if !started = false then
      begin
        Graphics.open_graph " 200x200";
        Graphics.set_window_title "WATCH";
        Graphics.auto_synchronize false;
      end


  let startstop_inputs () =
    init ();
    Graphics.button_down ()

  let startstop_outputs (h,m,s,start)=
    (* watch_outputs (h,m,s); *)
    let s = Printf.sprintf "%b %02d:%02d:%02d" start h m s in
    Graphics.clear_graph ();
    Graphics.moveto 100 100;
    Graphics.draw_string s;
    Graphics.synchronize ();
    Unix.sleepf 0.01
    (* Printf.printf "start = %b \n" start *)
end

(* let%node watch_noclock () ~return:(h,m,s) =
 *   s = 0 --> if (pre s) = 59 then 0 else (pre s + 1);
 *   m = 0 --> if s = 0 then if (pre m) = 59 then 0 else (pre m + 1) else (pre m);
 *   h = 0 --> if (m = 0 && s = 0) then if pre h = 11 then 0 else (pre h + 1) else pre h *)

let%node count d ~return:(cpt) =
  cpt = (0 --< (cpt+1) ) mod d

let%node watch (sec) ~return:(h,m,s) =
  no_s = count (100 --@ sec);
  min = (no_s = 0);
  no_m = count (60 --@ min);
  hour = (no_m = 0);
  no_h = count (12 --@ hour);
  h' = merge hour no_h ((pre h') --@ not hour);
  h = merge min h' (pre h --@ not min);
  m = merge min no_m ((pre m)  --@ not min);
  s = no_s

let%node startstop (b) ~return:(h,m,s,sec) =
  edge = false --> (b && (not (pre b)));
  sec = true --< if edge then not sec else sec;
  (h',m',s') = watch (sec --@ c);
  (* k = h' + m'; *)
  c = true;
  d = false;
  k = c --@ d;
  u = 2 --@ k;
  v = m' --@ c;
  (* w = (u + v) --@ not d ; *)
  (* w' = (u + v) --@ d ; *)
  (* h = merge sec h' ((0 --< h) --@ not sec); *)
  (* m = merge sec m' ((0 --< m) --@ not sec); *)
  (* s = merge sec s' ((0 --< s) --@ not sec); *)
  h = 0;
  m = 0;
  s = 0;
