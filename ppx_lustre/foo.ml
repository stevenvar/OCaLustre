let%node maison () (a,b,c) =
  b := true --> false; 
  c := pre a 
