record bob = { foo : string = "Hello";
               bar : string;
               mutable n : int = 1 }

record weird = { x : weird option = (Some (create_weird ~x:None ())) }

let _ =
  let x = create_bob ~bar:"World" () in
  x.n <- x.n + 1;
  Printf.printf "%s %s %i\n" x.foo x.bar x.n
