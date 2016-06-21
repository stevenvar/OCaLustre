
module Option = struct
  type 'a t = 'a option

  let get o = match o with
    | None -> failwith "No value"
    | Some x -> x
end

let%node naturels ~inf:(a,b,c) ~outf:(d,e,f) =
  d = 0 fby (n + 1) 
    
  
