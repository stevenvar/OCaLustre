let%node h x ~return:y =
  y = x

let%node f x ~return:y =
  y = 0 ->> (x+1)

let%node g (x,c) ~return:k =
  k = f (x [@when c])

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

let%node double_merge (a,b,c,d,e,f,g) ~return:(m,u,v) =
  (u,v) = ((merge a b c),(merge d e f));
  m = merge g u v

(* Expected to fail: *)
(* let%node not_ok (x,y,c,d) ~return:sum =
    a = x [@ when c];
    b = y [@ when d];
    sum = a + b *)

(* a and b do not compute the same thing: *)
let%node whenwhen (x,c) ~return:(a,b) =
    a = (2 fby x) [@when c];
    b = 2 fby (x [@when c])

(* Return flow with its clock: *)
let%node return_clock (x,y) ~return:(k,c) =
    c = (x && y);
    k = 2 [@when c]

(* Expected to fail because g is not returned with k: *)
let%node call_return_clock (x,y) ~return:(k) =
    (k,g) = return_clock (x,y)