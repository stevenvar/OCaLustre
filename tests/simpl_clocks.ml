(* Example from "A Formally Verified Compiler for Lustre" by Bourke et al. *)

let%node counter (ini,inc) ~return:n =
  res = false;
  n = if f || res then ini else c + inc;
  f = true ->> false;
  c = 0 ->> n

let%node d_integrator (gamma:int) ~return:(speed,position) =
  position = counter (1,2);
  speed = 2

(* let%node tracker (acc,limit) ~return:(p,t) =
 *   (s,p) = d_integrator(acc);
 *   x = (s > limit);
 *   c = counter (0 --@ x, 1 --@ x, false --@ x);
 *   t = merge x c (pt --@ not x);
 *   pt = 0 --< t *)
