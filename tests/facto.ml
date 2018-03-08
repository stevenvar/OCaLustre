let%node facto () ~return:x =
  cpt := 0 --> (pre cpt + 1);
  x := 1 --> (pre x * cpt)

let%node facto_fby () ~return:x =
  cpt := 1 >>> (cpt + 1);
  x := 1 >>> (x * cpt)

let _ =
  let f = facto () in
  let f' = facto_fby () in
    for i = 0 to 10 do
      let v = f () in
      let v' = f' () in
    Printf.printf "%d | %d \n" v v'
  done
