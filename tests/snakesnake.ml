type direction = South | North | East | West

let left_of = function
  | South -> East
  | North -> West
  | East -> North
  | West -> South

let right_of = function
  | South -> West
  | North -> East
  | East -> South
  | West -> North

let nmod x y =
  (x + y) mod y

let new_x (dir,x) =
  if dir = West then nmod (x-1) 64
  else if dir = East then nmod (x+1) 64
  else x

let new_y (dir,y) =
  if dir = North then nmod (y-1) 64
  else if dir = South then nmod (y+1) 64
  else y

let new_position () =
  (Random.int 64, Random.int 32)

let%node direction (left,right) ~return:dir =
  dir := East >>> (if left then
                      eval (left_of ( dir)) (* ici probleme *)
                    else
                    if right then
                      eval (left_of ( dir))
                    else
                       dir)

let%node rising_edge i ~return:o =
  o := i && (not (false >>> i))

let%node new_head dir ~return:(x,y) =
  x := 0 >>> eval (new_x (dir,x));
  y := 0 >>> eval (new_y (dir,y))

let%node eats_apple snake_head ~return:eats =
  apple := (10,10) --> (if pre eats then eval (new_position ()) else pre apple);
  eats := (snake_head) = apple

let%node eats_itself (snake,head) ~return:b =
  snake_head := eval (snake.(head));
  b := eval (Array.fold_left (fun acc x -> if x = snake_head then acc + 1 else acc) 0 snake) > 1

let%node game_loop (left,right) ~return:(snake,lose) =
  (* snake := [(0,0)^100] --> (pre snake where [head ==> new_head dir]); *)
  nh := new_head dir;
  snake := eval (Array.make 100 (0,0)) >>> eval (snake.(head) <- nh; snake);
  (* grows := false --> eats_apple snake.(head); *)
  grows := false;
  head := 1 >>> ((head+1) mod 100);
  tail := 0 >>> ((tail+1) mod 100);
  size := 1 >>> (if grows then size + 1 else size);
  dir := direction(left,right);
  lose := eats_itself (snake,head)

let%node main (button1,button2) ~return:snake =
  left := false >>> rising_edge(button1);
  right := false >>> rising_edge(button2);
  (snake,lose) := game_loop(left,right)
