
module Option = struct
  type 'a t = 'a option

  let get o = match o with
    | None -> failwith "No value"
    | Some x -> x
end

let%node truc (a,b,h) (z) =
  y := a on h;
  x := b on h;
  w := x + y;
  z := current w

let%node main () (z) =
  h := true --> (not (pre h) );
  a := 1 --> (pre a + 1);
  z := truc (a, 2, h)

let _ = 
  let main_step = main () in 
  while true do 
    let z = main_step () in
    print_int z;
    print_endline "";
    Unix.sleep 1 
  done 
