

(*
let%node id ~i:(x,f) ~o:y =
  y = x @wh f

let%node fibo ~i:() ~o:(g,f) =
  l = 1 ->> g;
  k = g + l;
  g = 0 ->> k;
  f = 0 ->> ( 1 --> (pre f + f ))
*)

(*)
let%node tictoc ~i:(c,d) ~o:(y,w) =
  y = 1 @wh c ;
  w = 2 @wh d

         *)

let%node tictac ~i:(c) ~o:(y) =
  a = 1 @wh c ;
  b = 2 @whnot c ;
  y = merge c a b



let _ =
  let tictoc_step = tictoc () in
  for i = 0 to 30 do
    let (v,_) = tictoc_step (i mod 2 = 0,34) in
    Printf.printf "%d \n" v
  done
