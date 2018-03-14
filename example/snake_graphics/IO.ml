let has_init = ref false

let init () =
  Graphics.open_graph " 500x500";
  Random.self_init ();
  Graphics.auto_synchronize false;
  has_init := true


let main_inputs () =
  if not !has_init then
    init ();
  if Graphics.key_pressed () then
    let c = Graphics.read_key () in
    match c with
    | 'd' -> (true,false)
    | 'q' -> (false,true)
    | _ -> (false,false)
  else
    (false,false)

let rtail = ref (0,0)
let rhead = ref (0,0)

let main_outputs (head,tail,apple,lose) =
  if lose then exit 0;
  Graphics.set_color (Graphics.red);
  let (x,y) = apple in
  Graphics.fill_rect (x*5) (y*5) 5 5;
  Graphics.set_color (Graphics.black);
  let (x,y) = head in
  let (xt,yt) = !rtail in
  Graphics.fill_rect (x*5) (y*5) 5 5;
  Graphics.set_color (Graphics.white);
  Graphics.fill_rect (xt*5) (yt*5) 5 5;
  Graphics.set_color (Graphics.black);
  rtail := tail;
  let (xt,yt) = !rtail in
  Graphics.fill_rect (xt*5) (yt*5) 5 5;
  Unix.sleepf 0.1;
  Graphics.synchronize ()

  (* Array.iter (fun (x,y) -> Printf.printf "(%d,%d)\n" x y) snake *)

let game_loop_inputs = main_inputs

let game_loop_outputs = main_outputs
