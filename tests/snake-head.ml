let%node rising_edge in_f ~return:out_f =
  out_f := in_f && (not (false ->> in_f))


let%node new_dir (dir,left,right) ~return:n_d =
  n_d := if left then (
      if (dir = South) then East
      else if (dir = North) then West
    else if (dir = East) then North
    else South)
    else if right then (
      if (dir = South) then West
      else if (dir = North) then East
      else if (dir = East) then South
      else North)
    else dir

      
let%node new_x (dir,x) ~return:n_x =
  n_x := (if dir = West then ( (x-1 + 64) mod 64)
          else if dir = East then ( (x+1 + 64) mod 64) else x)


let%node new_y (dir,y) ~return:n_y =
  n_y := (if dir = North then ( (y-1 + 32) mod 32)
    else if dir = South then ( (y+1 + 32) mod 32) else y)


let%node new_head (b1,b2) ~return:(x,y) = 
  left := false ->> rising_edge(b1);
  right := false ->> rising_edge(b2);
  dir := South ->> new_dir (dir, left, right);
  x := 0 ->> new_x(dir,x);
  y := 0 ->> new_y(dir,y)
