let%node inc x ~return:y =
  y := x + 1

let%node nat ()  ~return:n =
  n := 0 ->> inc n

let%node nat_sampled (s) ~return:(n,y) =
  y := inc (2 [@when s]);
  n := 0 ->> (n+1)
