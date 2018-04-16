let%node h x ~return:y =
  y = x

let%node f x ~return:y =
  y = 0 ->> (y+1)

let%node g (x,c) ~return:k =
  k =f (x [@when c])

let%node multi_sample (a,b,c,clock) ~return:t =
    t = (a [@when clock], b [@when clock], c [@when clock])

let%node ok (x,y,c) ~return:sum =
  a = x [@ when c];
  b = y [@ when c];
  sum = a + b

let%node merge_all (x,y,c) ~return:m =
  m = merge c y x

let%node call_merge_all (a,b,d) ~return:m =
  m = merge_all (a,b,d)

let%node double_merge (a,b,e,f,j,c,d,k) ~return:(m,u,v) =
  (u,v) = ((merge d a b),(merge k e j));
  m = merge c u v

(* let%node not_ok (x,y,c,d) ~return:sum =
 *   a = x [@ when c];
 *   b = y [@ when d];
 *   sum = a + b *)

(* 'a -> 'b *)
(* les sorties sont (potentiellement) + rapides que les entrées *)
let%node trois () ~return:x =
  x = 3

(* (c:'a) -> 'b *)
let%node call_trois (c) ~return:(x) =
  x = trois ( () [@when c] )
(* x est censé avoir une valeur que c soit vrai ou non ... mais là il faut nil si c est faux *)

(* Du coup on peut faire des opérations _ + nil ... *)
(* 'a -> 'a *)
let%node call_call (x) ~return:y =
  y = x + call_trois (false)


let%node whenwhen (x,c) ~return:(a,b) =
    a = (2 fby x) [@when c];
    b = 2 fby (x [@when c]);
