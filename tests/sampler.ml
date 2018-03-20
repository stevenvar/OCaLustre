module IO = struct
  let main_inputs () = ()
  let main_outputs (k) = print_int k
end

let%node merger (c,a,b) ~return:y =
   y := merge c a b

let%node call_merger (c,x,y) ~return:k =
  k := merger(c,(x [@ when c]),(y [@ whennot c]))

let%node main () ~return:k =
  tictoc := true >>> (false >>> tictoc);
  k := call_merger(tictoc,1,2)
