(*let%node fibonacci ~i:() ~o:(f) =
  f = 0 ->> ((1 ->> f) + f)

k = 1 ->> f 
f = 0 ->> (k + f)

*)

let fibo_0 () =
  let k = 1 in
  let f = 0 in (** assert f >= 0 **)
  let result = f in 
  let st_k = f in
  let st_f = k + f in
  let pre_f = f in 
  let state = st_k,st_f,pre_f in 
  state, result

let fibo_step state () =
  let st_k, st_f, pre_f = state in (** assume pre_f >= 0 **)
  let k = st_k in
  let f = st_f in
  let result = f in
  let st_k = f in
  let st_f = k + f in
  (** assert f >= 0 **)
  let state = st_k,st_f,f in 
  state, result

let contract_0 x =
  let y = 0 in
  let z = y in 
  let st_y = y + x in 
  let state = (y,st_y) in 
  (state, z)

let contract_step state x = 
  let (y,st_y) = state in
  let y = st_y in 
  let z = y in
  let st_y = y + x in 
  let state = (y,st_y) in 
  (state, z)

let fibonacci () =
  let st_f = ref 0  in
  let st__aux_1 = ref 1  in
  let fibonacci_step () =
    let _aux_1 = !st__aux_1  in
    let f = !st_f  in st__aux_1 := f; st_f := (_aux_1 + f); f  in
  fibonacci_step 

let _ =
  let (s,z) = fibo_0 () in
  let fibon = fibonacci () in
  Printf.printf "%d " z ; 
  let state = ref s in 
  for i = 1 to 10 do
    let (s,k) = fibo_step !state () in
    let v = fibon () in 
    Printf.printf "%d <%d> " k v ;
    state := s;
  done
