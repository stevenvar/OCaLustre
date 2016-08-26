

let%node naturels ~i:(a,b,c) ~o:(d,e,f) =
  ((a,d),f) = (a,b);
    e = c


let _ =
  let nat = naturels () in
  for i = 0 to 10 do
    let a,b,c = nat (1,2,3) in
    Printf.printf "---> %d \n" a
  done
