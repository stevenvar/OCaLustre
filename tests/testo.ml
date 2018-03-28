(* let%node test (x,y,c) ~return:(a,b) = *)
  (* a := x [@ when c]; *)
  (* b := y [@ when_not c] *)

(* let%node test_call (x,y,c) ~return:(i,j) = *)
  (* (i,j) := test(x,y,c) *)
(* k := merge c i j *)

module IO = struct
  let fil_inputs () = (Random.bool (), 1)
  let fil_outputs (z,k,u) =
    Unix.sleepf 1.;
    Format.printf "(%d,%d,%d)\n%!" z k u
end

let%node id x ~return:y =
  y = 0 fby (y+1)

let%node fil (c,y) ~return:(z,k,u) =
  z = id (y [@whennot c]);
  k = id (y [@when c]);
  u = merge c k z



(* let%node test (a,b,c) ~return:d =
 *   d := merge c a b
 *
 * let%node sample (c,y) ~return:k =
 *   k := y [@when c]
 *
 * let%node call_sample (c,d,y) ~return:(v,k) =
 *   v := c [@when d];
 *   k := sample(v,y)
 *
 * let%node unif (a,b,d,x) ~return:(y,p) =
 *   p := b && a;
 *   y := x [@when p]
 *
 * let%node call_unif (a,b,d,x) ~return:(y,p) =
 *   (y,p) = unif (a,b,d,x) *)
