module IO = struct
  let main_inputs () = ()
  let main_outputs (k) = print_int k
end

let%node merger (c,a,b) ~return:y =
   y := merge c a b

let%node call_merger (d,x,y) ~return:k =
  k := merger(d,(x [@ when d]),(y [@ whenot d]))

let%node main () ~return:k =
  tictoc := true ->> (false ->> tictoc);
  k := call_merger(tictoc,1,2)
