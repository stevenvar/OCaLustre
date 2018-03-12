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

exception Break
exception Lose
let collides_with_itself (snake,head,tail) =
  let i = ref tail in
  try
  while true do
    if !i = head then raise Break;
    if snake.(!i) = snake.(head) then
      raise Lose;
    incr i;
    i := !i mod Array.length snake;
  done; false
with Break -> false
   | Lose -> true
   | Invalid_argument _ -> Printf.printf "i= %d" !i; failwith "fuck"

let nmod x y =
  (x + y) mod y

let new_x (dir,x) =
  if dir = West then nmod (x-1) 100
  else if dir = East then nmod (x+1) 100
  else x

let new_y (dir,y) =
  if dir = North then nmod (y-1) 100
  else if dir = South then nmod (y+1) 100
  else y

let new_position () =
  (Random.int 64, Random.int 32)

let%node nx (dir,x) ~return:nx =
  nx := eval (new_x (dir,x))

let%node ny (dir,y) ~return:ny =
  ny := eval (new_y (dir,y))

let%node left (dir) ~return:ndir =
  ndir := eval (left_of dir)

let%node right (dir) ~return:ndir =
  ndir := eval (right_of dir)

let%node direction (left,right) ~return:dir =
  pre_dir := South >>> dir ;
  dir := South --> (if left then
                      left (pre_dir)
                    else
                    if right then
                      right (pre_dir)
                    else
                       (pre_dir))

let%node rising_edge i ~return:o =
  o := i && (not (false >>> i))

let%node new_head dir ~return:(x,y) =
  x := 0 --> nx (dir,pre x);
  y := 0 --> ny (dir,pre y)

let%node eats_apple snake_head ~return:(apple,eats) =
  apple := eval (new_position ()) --> (if pre eats then eval (new_position ()) else pre apple);
  eats := (snake_head) = apple

let%node eats_itself (snake,head,tail) ~return:b =
  b := false --> eval (collides_with_itself (snake,head,tail))

let%node maj_head (snake,head,new_head) ~return:s =
  s := eval (snake.(head) <- new_head ; snake )

let%node game_loop (left,right) ~return:(snake,head,tail,apple,lose) =
  (* snake := [(0,0)^100] --> (pre snake where [head ==> new_head dir]); *)
  nh := new_head dir;
  head := 1 >>> ((head+1) mod 100);
  snake := eval (Array.make 100 (0,0)) >>> maj_head(snake, head,nh);
(apple,grows) := eats_apple nh;
  tail := if not grows then (0 >>> ( (tail+1) mod 100)) else (0 >>> tail);
  size := 2 >>> (if grows then size + 1 else size);
  dir := direction(left,right);
  lose := eats_itself (snake,head,tail)

let%node main (button1,button2) ~return:(snake,head,tail,apple,lose) =
  left := false >>> rising_edge(button1);
  right := false >>> rising_edge(button2);
  (snake,head,tail,apple,lose) := game_loop(left,right)
