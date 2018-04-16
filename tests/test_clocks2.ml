let%node merger (x,y,c) ~return:z =
  z = merge c y x

let%node trois x ~return:t = (t = 3)

let%node call_trois (a,c,d) ~return:(t,v) =
  t = trois (a [@when c]);
  v = trois (a [@when d])
