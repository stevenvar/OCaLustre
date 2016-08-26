

let%node naturels ~i:((a,b),c) ~o:((v,w),z) =
  (n,u) = ((0 ->> 4),(0->>21));
  v = n+u;
  z = 1;
  w = 4



let _ =
  let nat = naturels () in
  for i = 0 to 10 do
    let (v,w),z = nat ((1,2),3) in
    Printf.printf "---> %d \n" v
  done
