open Pic 

let test =
  write_bit RB4 (if test_bit RB1 then 0 else 1);
  write_bit RD2 (if test_bit RD1 then 1 else 0)

let _ = test  
