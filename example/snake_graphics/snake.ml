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

(* exception Break
 * exception Lose
 * let collides_with_itself (snake,head,tail) =
 *   let i = ref tail in
 *   try
 *   while true do
 *     if !i = head then raise Break;
 *     if snake.(!i) = snake.(head) then
 *       raise Lose;
 *     incr i;
 *     i := !i mod Array.length snake;
 *   done; false
 * with Break -> false
 *    | Lose -> true *)

let nmod x y =
  (x + y) mod y

let new_position () =  (Random.int 100)

let%node new_x (dir,x) ~return:nx =
  nx = if dir = West then eval (nmod (x-1) 100)
  else if dir = East then eval (nmod (x+1) 100)
  else x

let%node new_y (dir,y) ~return:ny =
  ny = if dir = North then eval (nmod (y-1) 100)
  else if dir = South then eval (nmod (y+1) 100)
  else y

let%node left (dir) ~return:ndir =
  ndir = eval (left_of dir)

let%node right (dir) ~return:ndir =
  ndir = eval (right_of dir)

let%node direction (l,r) ~return:dir =
  pre_dir = (South ->> dir);
  dir = if l then
           (merge l (left (pre_dir [@when l])) (pre_dir [@whennot l]))
         else
           (merge r (right (pre_dir [@when r])) (pre_dir [@whennot r]))

let%node rising_edge i ~return:o =
  o = (i && (not (false ->> i)))

let%node new_head dir ~return:(x,y) =
  x = new_x (dir,0 ->> x);
  y = new_y (dir,0 ->> y)

(* let%node eats_apple (headx,heady) ~return:(eats) =
 *   (\* (apple_x,apple_y) = eval (new_position ()) ->> (if eats then eval (new_position ()) else (apple_x,apple_y)); *\)
 *   eats = ((headx,heady) = apple) *)

(* let%node eats_itself (snake,head,tail) ~return:b =
 *   b = false --> eval (collides_with_itself (snake,head,tail)) *)

let%node game_loop (l,r) ~return:(head,tail,nx,ny,apple_x,apple_y,size) =
  (nx,ny) = new_head dir;
  dir = direction(l,r);
  head = (1 ->> ((head+1) mod 10));
  eats = ( apple_x = nx && apple_y = ny);
  apple_x = eval (new_position ()) ->> if eats then eval (new_position ()) else apple_x;
  apple_y = (eval (new_position ()) ->> if eats then eval (new_position ()) else apple_y);
  tail = (if not eats then (0 ->> ((tail+1) mod 10)) else (0 ->> tail));
  size = (1 ->> (if eats then (size + 1) else size))

let%node main (button1,button2) ~return:(head,tail,nx,ny,apple_x,apple_y,size) =
  left = rising_edge (button1);
  right = rising_edge (button2);
  (head,tail,nx,ny,apple_x,apple_y,size) = game_loop(left,right)
