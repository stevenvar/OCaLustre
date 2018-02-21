let f = fun x -> x + 1
let g = fun x -> x - 1
let h = fun x -> x * 10

let%node compute f ~return:res =
  res := eval (f 4)

let _ =
  let cmp = compute () in
  List.iter(fun x ->
      print_int (cmp x)) [f;g;h]
