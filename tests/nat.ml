let%node incr x ~return:y =
  y := x + 1

let%node nat () ~return:n =
   n := 0 >>> incr n
