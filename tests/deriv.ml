let%node deriv x ~return: s =
  s = x - (0 ->> x)

let%node deriv2 x ~return: s =
  s = deriv (deriv (x))