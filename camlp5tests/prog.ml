open Pic;; 

node test = 
  RB4 := if RB1 then 0 else 1 ; 
  RD2 := if RD1 then 1 else 0 ;
  RB1 := pre RB3

let _ = test  
