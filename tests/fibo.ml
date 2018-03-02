
let%node fibonacci () ~return:(f) =
  f := 0 >>> ((1 >>> f) + f)
