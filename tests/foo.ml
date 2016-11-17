let%node g ~i:(x) ~o:(y:int) =
  y = x --> (x + 1) 

let%node truc ~i:(x:int) ~o:r =
  pre ( x >= 0) ; post (r >= 0); inv (y >= 0);
  (y:int) = 0 ->> (y + 1);
  ((r:int),(w:int)) = ((g x) , 23)

let%node fibonacci ~i:() ~o:(f,g) = 
  f = 0 ->> (1 --> f + (0 ->> f));
  g = 0 ->> ((1 ->> f) + f)

let _ =
  let fibo_step = fibonacci () in
  for i = 0 to 10 do
    let (f,g) = fibo_step () in
    Printf.printf "%d %d - " f g
  done
