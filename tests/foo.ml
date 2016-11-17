let%node g ~i:(x) ~o:(y:int) =
  y = x 

let%node truc ~i:(x:int) ~o:r =
  pre ( x >= 0) ; post (r >= 0); inv (y >= 0);
  (y:int) = 0 ->> (y + 1);
  ((r:int),(w:int)) = ((g x) , 23)

