let%node tictoc () ~return:y =
  c := true >>> (false >>> c);
  a := 1 [@ when c];
  b := 2 [@ whennot c];
  y := merge c a b


let _ =
  let fibonacci_step = fibonacci ()
  (* the call to fibonacci () initializes the node and returns the step function *)
  in
  for i = 0 to 30 do
    let v = fibonacci_step () in
    Printf.printf "%d \n" v
  done
