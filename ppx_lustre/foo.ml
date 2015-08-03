let%node xor (a,b) (xor, machin) =
  machin := xor;
  xor := if a then not b else b
