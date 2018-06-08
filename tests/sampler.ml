module IO = struct
  let main_inputs () = ()
  let main_outputs (k) = print_int k
end

let%node merger (c,a,b) ~return:y =
   y := merge c a b


(* call_merger ::   (base * base * base) -> base *)

let%node call_merger (d,x,y) ~return:k =
  k := merger(d,(x [@ when d]),(y [@ whenot d]))

let%node main (c) ~return:k =
  tictoc := true ->> (false ->> tictoc);
  k := call_merger(tictoc --@ c, 1 --@ c , 2 --@ c)
