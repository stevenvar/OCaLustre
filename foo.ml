

let%node naturels ~i:((a,b),c) ~o:(v) =
  (n,u) = ((0 ->> 4),(0->>21));
  v = n+u



let _ =
  let nat = naturels () in
  for i = 0 to 10 do
    let (v) = nat ((1,2),3) in
    Printf.printf "---> %d \n" v
  done
