

(*
let%node id ~i:(x,f) ~o:y =
  y = x @wh f

let%node fibo ~i:() ~o:(g,f) =
  l = 1 ->> g;
  k = g + l;
  g = 0 ->> k;
  f = 0 ->> ( 1 --> (pre f + f ))
*)


let%node test ~i:c ~o:y =

  a = 1 @wh c ;
  b = 2 @whnot c;
  y = merge c a b

(*
let%node test ~i:c ~o:y =
  y =  (1 @wh c) + (3 @wh c)

*)
let _ =
  let n = test () in
  let c = ref true in
  for i = 0 to 30 do
    c := not !c;
    let a= n !c  in
    Printf.printf "---> %d \n" a
  done
