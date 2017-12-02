(*Controller Module*)
open Lwt
open LTerm_widget
open LTerm_geom
open LTerm_text
open LTerm_key
open State
open Clview
open LTerm_style
open LTerm_draw
open LTerm_ui
open CamomileLibrary

(* type coord = LTerm_geom.coord *)
let rec loop ui coord =
  LTerm_ui.wait ui >>= function
  | LTerm_event.Key{ code = Escape; _ } ->
    return ()
  | _ ->
    loop ui coord

let main () =
  Lazy.force LTerm.stdout
  >>= fun term ->

  (* Coordinates of the message. *)
  let coord = ref { row = 0; col = 0 } in

  LTerm_ui.create term (fun matrix size -> Clview.draw matrix size !coord)
  >>= fun ui ->
  Lwt.finalize (fun () -> loop ui coord) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
