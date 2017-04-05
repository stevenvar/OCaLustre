let%node f ~i:() ~o:(x) =
  v = 4 ->> (5 ->> (6 ->> x));
  x = 1 ->> (2 ->> (3 ->> v))

let%node g ~i:() ~o:(u) =
  u = f ()

let _ =
  let s = g_0 () in
  print_int s.g_out_u;
  for i = 0 to 100 do
    g_next s ();
    print_int s.g_out_u;
  done
