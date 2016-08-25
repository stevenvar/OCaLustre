
(*let%node naturels ~i:() ~o:(d) =
  pre_d = 0 ->> d;
  tf = true ->> false;
  x = if tf then 1 else pre_d + d ;
  d = 1 ->> x*)


let%node naturels ~i:(x) ~o:(d,e) =
  e = 3 @> x ;
  d = 1 ->> 2

let _ =
  let nat = naturels () in
  for i = 0 to 10 do
    let d,e = nat () in
    Printf.printf "%d %d\n" d e;
  done
