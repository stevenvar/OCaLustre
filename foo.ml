

let%node nat ~i:(x,y,w,ck1,ck2,ck3) ~o:(z,k,r) =
  z = x @ ck1;
  k = y @ ck2;
  r = w @ ck3

let%node test ~i:(a,ck) ~o:x =
  x = a @ ck 

let _ =
  let n = nat () in
  for i = 0 to 10 do
    let (a,b,c) = n (1,3,2,2,2,3) in
    Printf.printf "---> %d %d\n" a b
  done
