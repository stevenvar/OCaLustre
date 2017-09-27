open Foo

let _ =
  let cal = caller (1,2,true) in
  for i = 0 to 20 do
    let c,e = cal (1,2,false) in
    print_int c
  done
