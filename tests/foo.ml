let%node toto ~i:() ~o:(x) =
  y = x + 1 ;
  x = 0 ->> y

let%node call ~i:() ~o:(y) =
  y = toto ()

let _ =
  let step = call () in
  for i = 0 to 10 do
    step ()
  done
