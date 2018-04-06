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


(* let%node not_ok (x,y,c,d) ~return:sum =
 *   a = x [@ when c];
 *   b = y [@ when d];
 *   sum = a + b *)

(* 'a -> 'b *)
let%node trois () ~return:x =
  x = 3

(* (c:'a) -> 'b *)
let%node call_trois (c) ~return:(x) =
  x = trois ( () [@when c] )
(* Quand c est faux, x vaut nil, mais on le voit pas dans le type de sortie ... *)

(* Du coup on peut faire des opérations _ + nil ... *)
(* 'a -> 'a *)
let%node call_call (x) ~return:y =
  y = x + call_trois (false)


let%node whenwhen (x,c) ~return:(a,b) =
    a = (2 fby x) [@when c];
    b = 2 fby (x [@when c]);
