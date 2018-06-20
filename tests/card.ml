

module IO = struct

  let bool_of_int = function
    | 0 -> false
    | _ -> true

  let print_bool = function
    | false -> Printf.printf "0"
    | true -> Printf.printf "1"

  let main_inputs () =
    try Scanf.scanf "%d %d\n" (fun c d -> (bool_of_int c, bool_of_int d)) with
    _ -> Printf.printf "\n" ; exit 0

  let main_outputs () = ()
end

(** the punchcard has two lines of holes :

>>>>>>>>>>>>>>>>>>>>>>>>>
|                       |
| clk  o o o o o o o o  |
|                       |
| data o     o o   o    |
|                       |
+-----------------------+

The line on top (clk) represents the clock

The line below represents the data that is present when the clock is true

In this example, the card reads (from left to right) 10011010


 **)

let%node edge x ~return:e =
  e = (x && (not (true ->> x)))

let%node print_data b ~return:p =
  p = eval (IO.print_bool b)

let%node read_card (top,bot) ~return:(clk,d) =
  clk = edge top;
  d = bot --@ clk

let%node main (top,bot) ~return:(p) =
  (clk,d) = read_card (top,bot);
  p = print_data d
