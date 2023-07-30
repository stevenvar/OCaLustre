open Avr

let t = Array.make 8 false
let clk  = PIN13
let data = PIN12
let leds = [| PIN42 ; PIN43; PIN44; PIN45; PIN46; PIN47; PIN48; PIN49 |]

let init () =
  (* réglage des broches en entrée *)
  pin_mode clk INPUT;
  pin_mode data INPUT;
  (* réglage des broches en sortie *)
  Array.iter (fun x -> pin_mode x OUTPUT) leds

let level_of_bool = function false -> LOW | true -> HIGH
let bool_of_level = function LOW -> false | HIGH -> true

let input_clk () = bool_of_level (digital_read clk)
let input_data () = bool_of_level (digital_read data)

let output i data clk send =
  if clk then t.(i) <- data;
  if send then
    Array.iteri (fun i x -> digital_write leds.(i) (level_of_bool x)) t

let%node edge x ~return:e =
  e = (x && (not (true ->> x)))

let%node read_bit (top,bot) ~return:(clk,data) =
  clk = edge top;
  data = bot [@when clk]

let%node count (reset) ~return:(cpt) =
  cpt = (0 ->> (cpt + 1)) mod reset

let%node read_card (top,bot) ~return:(i,data,clk,send) =
  (clk,data) = read_bit (top,bot);
  i = count(8 [@when clk]);
  send = merge clk (i = 7) false

let () =
  (* initialisation du matériel *)
  init ();
  (* lecture des entrées *)
  let c = input_clk () in
  let d = input_data () in
  (* création de l'état du noeud principal *)
  let st = read_card_init c d in
  (* émission des sorties *)
  let i = st.read_card_out_i in
  let data = st.read_card_out_data in
  let clk = st.read_card_out_clk in
  let send = st.read_card_out_send in
  output i data clk send;
  while true do
    (* lecture des entrées *)
    let c = input_clk () in
    let d = input_data () in
    (* mise à jour de l'état du noeud principal *)
    read_card_step st c d;
    (* émission des sorties *)
    let i = st.read_card_out_i in
    let data = st.read_card_out_data in
    let clk = st.read_card_out_clk in
    let send = st.read_card_out_send in
    output i data clk send
  done
