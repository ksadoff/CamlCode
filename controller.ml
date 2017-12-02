(* The Controller module is responsible for getting input from the user
 * either as keybindings within the file or as a command in the command
 * prompt. It then uses the State module to update the current state of
 * the editor and the CLView module to update the display. *)

open Lwt
open LTerm_geom
open LTerm_text
open LTerm_key

let rec loop ui coord =
  LTerm_ui.wait ui >>= function
    | LTerm_event.Key{ code = Escape; _ } ->
        return ()
    | _ ->
        loop ui coord

let draw ui matrix coord =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  if size.rows > 2 && size.cols > 2 then begin
    let ctx = LTerm_draw.sub ctx { row1 = 1; col1 = 1; row2 = size.rows - 1; col2 = size.cols - 1 } in
    LTerm_draw.draw_styled ctx coord.row coord.col (eval [B_fg LTerm_style.lblue; S"Move me"; E_fg])
  end

let main () =
  Lazy.force LTerm.stdout
  >>= fun term ->

  (* Coordinates of the message. *)
  let coord = ref { row = 0; col = 0 } in

  LTerm_ui.create term (fun matrix size -> draw matrix size !coord)
  >>= fun ui ->
  Lwt.finalize (fun () -> loop ui coord) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
