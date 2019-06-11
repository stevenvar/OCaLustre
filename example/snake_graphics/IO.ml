let has_init = ref false

let init () =
  Graphics.open_graph " 500x500";
  Random.self_init ();
  Graphics.auto_synchronize false;
  has_init := true

let snake = Array.init 10 (fun x -> (0,0))

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

let draw_snake (head,tail) =
  for i = head to tail do
    let (x,y) = snake.(i) in
    Graphics.fill_rect (x*5) (y*5) 5 5
  done

let main_outputs (head,tail,nx,ny,applex,appley) =
  (* if lose then exit 0; *)
  snake.(head) <- (nx,ny)
  (* Graphics.set_color (Graphics.red);
   * let (x,y) = applex,appley in
   * Graphics.fill_rect (x*5) (y*5) 5 5;
   * Graphics.set_color (Graphics.black);
   * let (x,y) = head in
   * let (xt,yt) = !rtail in
   * Graphics.fill_rect (x*5) (y*5) 5 5;
   * Graphics.set_color (Graphics.white);
   * Graphics.fill_rect (xt*5) (yt*5) 5 5;
   * Graphics.set_color (Graphics.black);
   * rtail := tail;
   * let (xt,yt) = !rtail in
   * Graphics.fill_rect (xt*5) (yt*5) 5 5;
   * Unix.sleepf 0.1;
   * Graphics.synchronize () *)

  (* Array.iter (fun (x,y) -> Printf.printf "(%d,%d)\n" x y) snake *)

let game_loop_step_in = main_inputs

let game_loop_step_out = main_outputs
