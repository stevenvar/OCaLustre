let%node tictoc c ~return:y =
  a := 1 [@ when c];
  b := 2 [@ whennot c];
  y := merge c a b

let%node call_tictoc () ~return:d =
  c := true >>> (false >>> c);
  d := tictoc c

let _ =
  let c = call_tictoc () in
  for i = 0 to 10 do
    let x = c () in
    print_int x;
  done
