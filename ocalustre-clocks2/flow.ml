type 'a flow = None | Value of 'a

let get f = match f with
  | None -> failwith "No value"
  | Value v -> v 
