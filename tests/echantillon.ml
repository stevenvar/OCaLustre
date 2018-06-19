let%node fil (x) ~return:y =
  y = x

let%node call_fil (x,c) ~return:(y,z) =
  y = fil(x);
  z = fil(x --@ c)

let%node sampler (x,c) ~return:y =
  y = x --@ c

let%node sampler_inv (x,c) ~return:y =
  y = x --@ not c

let%node sampler2 x ~return:(y,c) =
  c = true;
  y = x --@ c


let%node call_sampler (k) ~return:(y,p,q) =
  y = sampler(1,k);
  (p,q) = sampler2(4 --@ k)

let%node cpt (x) ~return:y =
  y = x

let%node watch (sec) ~return:(min,hour,h,m,s) =
  s = cpt (60 [@when sec]);
  min = (s = 60);
  m = cpt (60 [@when min]);
  hour = (m = 60);
  h = cpt (24 [@when hour])

let%node ex_const ( v,c,d ) ~return:x =
  w = merge d v (0 [@when c] [@whennot d]);
  x = merge c w 1

(* let%node ex_const ( v,c,d ) ~return:x = *)
(* w = merge d v ((0 [@when c]) [@whennot d]); *)
(* x = merge c w (1 [@when c ]) *)

(* let%node call_sampler () ~return:(y,w,z) =
 *   k = true;
 *   y = sampler(1,k);
 *   w = sampler_inv(2,k);
 *   z = merge k y w
 *
 * let%node call_sampler2 () ~return:(y) =
 *   (y,k) = sampler2(1) *)

let%node merger (x,y,z) ~return:(k,v) =
  v = y;
  k = merge x y z
let%node call_sampler_slower (d,e) ~return:w =
  (* d = c [@when e]; *)
  w = sampler(8 [@when e],d)
