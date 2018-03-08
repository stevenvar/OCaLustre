
let main_inputs () = (true,false)

let main_outputs snake =
  Array.iter (fun (x,y) -> Printf.printf "(%d,%d)\n" x y) snake
