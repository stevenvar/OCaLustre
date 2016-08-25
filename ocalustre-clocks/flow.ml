type 'a flow = Nil | Val of 'a

let get = function
  | Nil -> failwith "not initialized"
  | Val x -> x
