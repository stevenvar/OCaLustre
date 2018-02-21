let%node nind () ~return:(y:int) =
  inv (x = y);
  (x:int) := 3;
  (y1:int) := 3 fby x;
  (y2:int) := 3 fby y1;
  (y3:int) := 3 fby y2;
  (y:int) := 3 fby y3
