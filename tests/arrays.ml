let%node cpt () ~return:c =
  c := 0 --> ((pre c) + 1)

let%node mk_array () ~return:a =
  a := [| (0,0)^5 |] --> ( (pre a) where (1 => (1,1) ; 2 => (2,2) ))

let _ =
  let main = mk_array () in
  for i = 0 to 10 do
    let a = main () in
    Array.iter (fun (x,y) -> Printf.printf "(%d,%d)" x y) a;
    Printf.printf "\n";
  done
