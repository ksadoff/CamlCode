(* Controller Module *)

open Lwt
open LTerm_geom
open State
open Clview
open CamomileLibrary

(* [repl ui stref] waits for input from the user. Once input is recieved it
 * creates a new state, updates the ui according to the new state
 * (by calling update in CLView) and passes the
 * new state to another call of [repl] recursively *)
let rec repl ui stref =
  LTerm_ui.wait ui >>= fun event ->
  match event with
  | LTerm_event.Key{ code = Escape; _ } ->
    return ()
  | _ -> 
    stref := Plugin.respond_to_event event !stref;
    LTerm_ui.draw ui;
    repl ui stref

(* Function to be called when starting the text editor with no files open.
 * The argument should be optional, when not given a string the
 * function is the same as if the file was not found *)
let main () =
  (* TODO: Right now, a somelines.txt is automatically opened.
   * We need to open [empty_state] by default instead, and then have
   * the user choose the file with a command. *)
  let stref' = empty_state
    |> fun st -> open_file st "../../testtxts/somelines.txt" in

  let stref = open_file stref' "../../testtxts/easy.txt" |> fun x -> ref x in
  Lazy.force LTerm.stdout
  >>= fun term -> Clview.draw term stref
  >>= fun ui ->
    Lwt.finalize (fun () -> repl ui stref) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
