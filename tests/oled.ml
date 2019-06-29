(* communication with OLED display in OCaLustre *)

type pin_val = HIGH | LOW


let bop = function HIGH -> true | LOW -> false


module IO = struct
  let main_inputs () = ()
  let main_outputs (cs,dc,spi) =
    Printf.printf "%b,%b,%d \n" (bop cs) (bop dc) spi;
end


let%node command_mode () ~return:(cs,dc) =
  cs := LOW;
  dc := LOW

let%node data_mode () ~return:(cs,dc) =
  cs := HIGH;
  dc := LOW

let%node send_lcd_command com ~return:(cs,dc,spi) =
  (cs,dc) := command_mode ();
  spi := com

let%node send_lcd_data dat ~return:(cs,dc,spi) =
  (cs,dc) := data_mode ();
  spi := dat

let%node clear () ~return:(spi_transfer,continue) =
  cpt := 0 fby (cpt+1);
  continue := cpt < 1024;
  spi_transfer := 0 [@ when continue]

let%node boot () ~return:(cs,dc,spi,ok) =
   (cs,dc,spi) := send_lcd_data(boot);
   ok := (boot = 0x00);
   boot :=  0xD5 >>> (0xF0 >>> (0x8D >>> (0x14 >>>
               (0xA1 >>> (0xC8 >>> (0x81 >>> (0xCF >>> (0xD9 >>>
               (0xF1 >>> (0xAF >>> (0x20 >>> 0x00)))))))))))

let%node game () ~return:(cs,dc,spi) =
  (cs,dc,spi) := send_lcd_data 42

let%node main () ~return:(cs,dc,spi) =
  clk := false >>> boot_ok;
  (cs1,dc1,spi1,boot_ok) := boot ();
  (cs2,dc2,spi2) := game ();
  (cs,dc,spi) := merge clk (cs2,dc2,spi2) (cs1,dc1,spi1)

(* let%node boot () ~return:(rst,cs,dc,spi,finish) =
 *   finish := boot_ptr > boot.size();
 *   rst := HIGH >>> (LOW >>> HIGH);
 *   (cs,dc,spi) := send_lcd_data(boot.(boot_ptr));
 *   boot_ptr := 0 >>> boot_ptr + 1 [@whennot finish];
 *   boot :=  [|
 *     0xD5 ; 0xF0 ; (\* Set display clock divisor = 0xF0 *\)
 *     0x8D ; 0x14; (\* Enable charge Pump *\)
 *     0xA1 ; (\* Set segment re-map *\)
 *     0xC8 ; (\* Set COM Output scan direction *\)
 *     0x81; 0xCF; (\* Set contrast = 0xCF *\)
 *     0xD9; 0xF1; (\* Set precharge = 0xF1 *\)
 *     0xAF; (\* Display ON *\)
 *     0x20; 0x00 (\* Set display mode = horizontal addressing mode *\)
 *   |]
 *
 * let%node main ~return:(rst,cs,dc,spi) =
 *   (r,c,d,s,finish) := boot();
 *   c = draw(2,2,true);
 *   (rst,cs,dc,spi) := merge ((r,c,d,c) [@when finish]) ((r,c,d,s) [@whennot finish]) *)
