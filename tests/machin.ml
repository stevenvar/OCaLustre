let%node fibonacci ~i:() ~o:(f) =
  f = 0 ->> ((1 ->> f) + f)
