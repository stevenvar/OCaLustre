type direction =
  | South 
  | North 
  | East 
  | West 
let left_of =
  function
  | South  -> East
  | North  -> West
  | East  -> North
  | West  -> South 
let right_of =
  function
  | South  -> West
  | North  -> East
  | East  -> South
  | West  -> North 
let nmod x y = (x + y) mod y 
let new_x (dir,x) =
  if dir = West
  then nmod (x - 1) 64
  else if dir = East then nmod (x + 1) 64 else x 
let new_y (dir,y) =
  if dir = North
  then nmod (y - 1) 64
  else if dir = South then nmod (y + 1) 64 else y 
let new_position () = ((Random.int 64), (Random.int 32)) 
let direction () =
  let st_dir = ref South  in
  let direction_step (left,right) =
    let dir = !st_dir  in
    st_dir :=
      (if left then left_of dir else if right then left_of dir else dir);
    dir  in
  direction_step 
let rising_edge () =
  let st__aux_1 = ref false  in
  let rising_edge_step i =
    let _aux_1 = !st__aux_1  in
    let o = i && (not _aux_1)  in st__aux_1 := i; o  in
  rising_edge_step 
let new_head () =
  let st_y = ref 0  in
  let st_x = ref 0  in
  let new_head_step dir =
    let x = !st_x  in
    let y = !st_y  in
    st_x := (new_x (dir, x)); st_y := (new_y (dir, y)); (x, y)  in
  new_head_step 
let eats_apple () =
  let st__aux_1 = ref true  in
  let st__aux_2 = ref (Obj.magic ())  in
  let st__aux_3 = ref (Obj.magic ())  in
  let eats_apple_step snake_head =
    let _aux_3 = !st__aux_3  in
    let _aux_2 = !st__aux_2  in
    let _aux_1 = !st__aux_1  in
    let apple =
      if _aux_1 then (10, 10) else if _aux_2 then new_position () else _aux_3
       in
    let eats = snake_head = apple  in
    st__aux_3 := apple; st__aux_2 := eats; st__aux_1 := false; eats  in
  eats_apple_step 
let eats_itself () =
  let eats_itself_step (snake,head) =
    let snake_head = snake.(head)  in
    let b =
      (Array.fold_left
         (fun acc  -> fun x  -> if x = snake_head then acc + 1 else acc) 0
         snake)
        > 1
       in
    b  in
  eats_itself_step 
let game_loop () =
  let st_size = ref 1  in
  let st_tail = ref 0  in
  let st_head = ref 1  in
  let st__aux_1 = ref true  in
  let st__aux_2 = ref (Obj.magic ())  in
  let eats_itself2_step = eats_itself ()  in
  let direction1_step = direction ()  in
  let game_loop_step (left,right) =
    let _aux_2 = !st__aux_2  in
    let _aux_1 = !st__aux_1  in
    let snake = if _aux_1 then Array.make 100 (0, 0) else _aux_2  in
    let grows = false  in
    let head = !st_head  in
    let tail = !st_tail  in
    let size = !st_size  in
    let dir = direction1_step (left, right)  in
    let lose = eats_itself2_step (snake, head)  in
    st__aux_2 := snake;
    st__aux_1 := false;
    st_head := (head + 1);
    st_tail := (tail + 1);
    st_size := (if grows then size + 1 else size);
    snake  in
  game_loop_step 
let main () =
  let st_right = ref false  in
  let st_left = ref false  in
  let game_loop3_step = game_loop ()  in
  let rising_edge2_step = rising_edge ()  in
  let rising_edge1_step = rising_edge ()  in
  let main_step (button1,button2) =
    let _aux_1 = rising_edge1_step button1  in
    let _aux_2 = rising_edge2_step button2  in
    let left = !st_left  in
    let right = !st_right  in
    let snake = game_loop3_step (left, right)  in
    st_left := _aux_1; st_right := _aux_2; snake  in
  main_step 
let () =
  let input_main () =
    let (button1,button2) = IO_main.inputs ()  in (button1, button2)  in
  let output_main snake = IO_main.outputs snake  in
  let main = main ()  in
  while true do
    let inputs = input_main ()  in
    let outputs = main inputs  in output_main outputs done
  
