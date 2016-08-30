

let%node nat ~i:(x,z) ~o:y =
  w = x + (2 @ z);
  y = 0 @ x




let _ =
  let n = nat () in
  for i = 0 to 10 do
    let a = n () in
    Printf.printf "---> %d %d %d\n" a a a
  done
