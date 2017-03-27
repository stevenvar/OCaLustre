

let%node foo ~i:(a,b) ~o:(c,d) =
  c = 0 --> a;
  d = 1 ->> (2 ->> b)

let%node call ~i:(x) ~o:(y) =
  y = foo (x,2)

let _ =
  let step = foo (1,2) in
  for i = 0 to 10 do
    step (1,2)
  done
