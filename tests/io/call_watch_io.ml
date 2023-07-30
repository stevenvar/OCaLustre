let init_call_watch () = ()
let input_call_watch () = true
let output_call_watch (h,m,s) =
  Format.printf "%02d:%02d:%02d\n%!" h m s;
  Unix.sleepf 0.0001
