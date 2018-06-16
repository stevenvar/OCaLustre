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


let%node call_sampler (k) ~return:(y) =
  (* k = true; *)
  y = sampler(1,k)

(* let%node call_sampler () ~return:(y,w,z) =
 *   k = true;
 *   y = sampler(1,k);
 *   w = sampler_inv(2,k);
 *   z = merge k y w
 *
 * let%node call_sampler2 () ~return:(y) =
 *   (y,k) = sampler2(1)
 *
 * let%node merger (x,y,z) ~return:(k,v) =
 *   v = y;
 *   k = merge x y z  *)
