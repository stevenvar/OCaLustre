
let%node fibonacci () ~return:(f) =
  f := 0 >>> ((1 >>> f) + f)



let _ =
  let fibonacci_step = fibonacci ()
  (* the call to fibonacci () initializes the node and returns the step function *)
  in
  for i = 0 to 30 do
    let v = fibonacci_step () in
    Printf.printf "%d \n" v
  done
