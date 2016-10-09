

(*
let%node id ~i:(x,f) ~o:y =
  y = x @wh f

let%node fibo ~i:() ~o:(g,f) =
  l = 1 ->> g;
  k = g + l;
  g = 0 ->> k;
  f = 0 ->> ( 1 --> (pre f + f ))
*)

(*
let%node tictoc ~i:(c,d) ~o:(y,w) =
  y = 1 @wh c ;
  w = 2 @wh d

         *)

(*

let%node tictac ~i:(c) ~o:(y) =
  a = 1 @wh c ;
  b = 2 @whnot c ;
  y = merge c a b

let%node tictoc ~i:(d,e) ~o:(y,z) =
    y = tictac d;
    z = tictac e


let _ =
  let tictoc_step = tictoc () in
  for i = 0 to 30 do
    let (v,w) = tictoc_step (true,false) in
    Printf.printf "%d \n" v
  done

*)

(*
let%node testo ~i:(x) ~o:(m) =
  n = true ->> false ;
  m = if n then x else (0 ->> m + 1)
*)
(*

let%node nat ~i:() ~o:(n) =
  n = 0 ->> (n+1)

*)

(*
let%node tictoc ~i:(c) ~o:(k) =

  n = (testo 12) @wh c;
  m = (testo 38)  @whnot c;
  k = merge c n m
*)

let%node test ~i:(x) ~o:(y) =
  y = x ->> 12

let%node user ~i:(x) ~o:(y) =
  y = test v;
  v = if x then 3 else 4 

let _ =
  let user_step = user false in
  for i = 0 to 30 do
    let v = user_step (false) in 
    Printf.printf "%d \n" v
  done
