
let%node fibo ~i:() ~o:(f) =
  f = 0 ->> ( 1 --> (pre f + f ))

let%node test ~i:(i) ~o:(c,o) =
  c = true --> (not (pre c));
  o = i @ c

let _ =
  let n = fibo () in
  for i = 0 to 30 do
    let a= n () in
    Printf.printf "---> %d \n" a
  done
