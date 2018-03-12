let%node mk_array () ~return:a =
  a := [| (0,0)^10 |] >>> [| a where (a.(1) <- (2,1) ; a.(3) <- (4,4)) |]
