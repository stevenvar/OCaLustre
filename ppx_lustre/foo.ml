let%node xor (a,b) (xor) =
  xor := if a then not b else b 
