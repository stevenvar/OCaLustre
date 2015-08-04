let%node xor (a,b) (xor, machin) =
  machin := a next b; 
  xor := if (pre b) then not b else b;
  
