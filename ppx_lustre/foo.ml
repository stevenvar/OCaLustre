let%node xor (a,b) (xor, machin) =
  machin := a next xor ; 
  xor := if (pre b) then not b else pre machin;
  
