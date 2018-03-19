let%node inc x ~return:y =
  y := x + 1

let%node nat () ~return:n =
   n := 0 >>> inc n
