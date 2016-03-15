
module Option = struct
  type 'a t = 'a option

  let get o = match o with
    | None -> failwith "No value"
    | Some x -> x
end

let%node naturels (a,b) (n, v, t) =
  v = n on a ;
  n = 0 fby (n + 1);
  t = (0 on a) + (1 on a)  
  
