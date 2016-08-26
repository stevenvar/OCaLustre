

let%node naturels ~i:(a,b,c) ~o:(d,e,f) =
  (d,e,f) = (a,b,c)



let _ =
  let nat = naturels () in
  for i = 0 to 10 do
    let a,b,c = nat (1,2,3) in
    Printf.printf "---> %d %d %d\n" a b c
  done
