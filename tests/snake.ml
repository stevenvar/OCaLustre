let%node rising_edge in_f ~return:out_f =
  out_f := in_f && (not (false ->> in_f))

let%node button_led button_pinin ~return:led_pinout =
  button_pressed := rising_edge(button_pinin);
  led_pinout := false ->> (if button_pressed then (not led_pinout) else led_pinout)

let _ =
  let pb = Printf.printf "%b" in
  let re = rising_edge false in
  pb (re false);
  pb (re true);
  pb (re false);
  pb (re true);
  pb (re true)
  

(*
let%node button_led button_pinin ~return:led_pinout = button_pressed := rising_edge(button_pinin); led_pinout := false ->>
(if button_pressed then (not led_pinout) else led_pinout)
*)
