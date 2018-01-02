(* let%node plus (a,b) ~return:(c) =
 *   c = a + b
 *
 * (\* let%node call ~i:(a,b) ~o:(v) = *\)
 * (\*   v = plus(a,b) *\)
 *
 * let%node call_A (a,b) ~return:(c,d) =
 *   c = plus (a,b);
 *   d = plus (a,b)
 *
 * let%node call_B (a,b) ~return:c =
 *   (c,d) = call_A(a,b)
 *
*)

let pb =
  function
    true -> print_endline "true"
  | _ -> print_endline "false"

let%node edge x ~return:y =
  y := x && (not (false ->> x))

let%node perforated (clk,v) ~return:d =
  e := edge clk;
  d := v [@ when e]


let _ =
  let f = false in
  let t = true in
  let edge = edge f in
  pb (edge f);
  pb (edge f);
  pb (edge t);
  pb (edge t);
  pb (edge f);
  pb (edge t)



(* let%node abro (a,b,r) ~return:o = *)
(*   o = edge(seenA && seenB); *)
(*   seenA = false --> (not R && (A || (pre seenA))); *)
(*   seenB = false --> (not R && (B || (pre seenB))); *)


(* let%node whn (a,c) ~return:b = *)
(*   b = a [@ when c] *)


(* let%node merge (a,k,b,c) ~return:(f,g) = *)
(*   (f,g) = ((merge c a b),(whn (k,c))) *)

(* let%node caller (u,k,v,o) ~return:(d,e) = *)
(*   (d,e) = merge (u,k,v,o) *)

(* let%node nat () ~return:n =
 *   n = 0 ->> (n+1)
 *
 * let%node facto () ~return:f =
 *   f = 1 ->> (f * cpt);
 *   cpt = 1 + nat (); *)
