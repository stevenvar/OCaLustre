
type 'a flow = None | Nil | Value of 'a    

module Flow = struct
  type 'a t = 'a flow 

  let get o = match o with
    | None -> failwith "No value"
    | Nil -> failwith "Nil"
    | Value x -> x
end

let%node test (a,b,c) (x,y,z) = 
	x := a on b ;
	y := c on b ;
	z := x + y 

let _ = 
	let fibo_step = test () in 
	while true do 
	let (x,y,z) = fibo_step (Value 1, Value true, Value 2 ) in
	print_int (Flow.get z) ;
	print_endline ""; 
	Unix.sleep 1 ;
done 