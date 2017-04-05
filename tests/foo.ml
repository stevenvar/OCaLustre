let%node f ~i:(a) ~o:(x) =
  v = x + 1;
  x = a ->> v

let%node g ~i:() ~o:(u) =
  u = f 10


let g_out u =
  print_int u;
  print_newline ()

let _ =
  let s = g_0 () in
  g_out (s.g_out_u);
  for i = 0 to 10 do
    g_next s ();
    g_out (s.g_out_u);
  done
