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

let nmod x y =
  (x + y) mod y

let new_position () =
  (Random.int 100, Random.int 100)

let%node nx (dir,x) ~return:nx =
  nx := if dir = West then eval (nmod (x-1) 100)
  else if dir = East then eval (nmod (x+1) 100)
  else x

let%node ny (dir,y) ~return:ny =
  ny := if dir = North then eval (nmod (y-1) 100)
  else if dir = South then eval (nmod (y+1) 100)
  else y

let%node left (dir) ~return:ndir =
  ndir := eval (left_of dir)

let%node right (dir) ~return:ndir =
  ndir := eval (right_of dir)

let%node direction (left,right) ~return:dir =
  dir := South --> (if left then
                      left (pre dir)
                    else
                    if right then
                      right (pre dir)
                    else
                       (pre dir))

let%node rising_edge i ~return:o =
  o := i && (not (false >>> i))

let%node new_head dir ~return:(x,y) =
  x := nx(dir,0 >>> x);
  y := ny(dir,0 >>> y)

let%node eats_apple snake_head ~return:(apple,eats) =
  apple := eval (new_position ()) >>> (if eats then eval (new_position ()) else apple);
  eats := (snake_head = apple)

let%node eats_itself (snake,head,tail) ~return:b =
  b := false --> eval (collides_with_itself (snake,head,tail))

let%node maj_head (snake,head,new_head) ~return:s =
  s := snake.update (head => new_head)

let%node game_loop (left,right) ~return:(hd,tl,apple,lose) =
  snake := [| (0,0)^100 |] --> (pre snake).update(head => nh);
  nh := new_head dir;
  dir := direction(left,right);
  head := 1 >>> ((head+1) mod 100);
  (apple,grows) := eats_apple nh;
  tail := if not grows then (0 >>> ((tail+1) mod 100)) else (0 >>> tail);
  size := 2 >>> (if grows then size + 1 else size);
  lose := eats_itself (snake,head,tail);
  hd := snake.(head);
  tl := snake.(tail)

let%node main (button1,button2) ~return:(hd,tl,apple,lose) =
  left := rising_edge(button1);
  right :=rising_edge(button2);
  (hd,tl,apple,lose) := game_loop(left,right)
