
type 'a flow = None | Nil | Value of 'a    

module Flow = struct
  type 'a t = 'a flow 

  let map f flow1 = 
	match flow1 with
    | None -> None
    | Nil -> Nil
    | Value x -> Value (f x)

  let map2 f flow1 flow2 = 
	match flow1,flow2 with
    | None, _ -> None
    | Nil, _ -> Nil
    | _, None -> None
    | _, Nil -> Nil
    | Value x, Value y  -> Value (f x y)

  let get o = match o with
    | None -> failwith "No value"
    | Nil -> failwith "Nil"
    | Value x -> x
end

let%node clocks (u,v,b) (z) = 
  x := u on b ;
  y := v on b ; 
  z := x * y 

let%node entiers (a,b) (n,m) = 
  m := a + b ; 
  n := 0 --> (pre n + 1)

let%node fibonacci () (f) = 
  f := 1 --> (pre (1 --> (f + pre f))) 

let _ = 
	let clocks_step = clocks () in 
	while true do 
	let z = clocks_step (Value 1, Value 2, Value true) in
	print_int (Flow.get z) ;
	print_endline "";
	Unix.sleep 1 
done 