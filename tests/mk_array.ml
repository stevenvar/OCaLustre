module IO = struct
  let mk_array_inputs () = (2,3)
  let mk_array_outputs (k,a) =
    print_int k;
    print_newline ();
    Array.iter print_int a;
    print_newline ();
    Unix.sleep 1

end

let%node mk_array (x,n) ~return:(k,a) =
  a := [| 2 ^ 10 |] >>> (a.map(fun x -> x + 1));
  k := a.fold((+),0)
