(*Controller Module*)
open Lwt
open LTerm_geom
open State
open Clview

(* [repl ui s] waits for input from the user. Once input is recieved it 
 * creates a new state, updates the ui according to the new state 
 * (by calling update in CLView) and passes the
 * new state to another call of [repl] recursively *)
let rec repl ui state =
  LTerm_ui.wait ui >>= function
  | LTerm_event.Key{ code = Escape; _ } ->
    return ()
  | _ ->
    repl ui state

(* Function to be called when starting the text editor with no files open.
 * The argument should be optional, when not given a string the
 * function is the same as if the file was not found *)
let main () = 
  (* TODO: Right now, a somelines.txt is automatically opened.
   * We need to open [empty_state] by default instead, and then have
   * the user choose the file with a command. *)
  let st = empty_state 
    |> fun st -> open_file st "../../testtxts/somelines.txt" in
  Lazy.force LTerm.stdout
  >>= fun term -> Clview.draw term st
  >>= fun ui ->
    Lwt.finalize (fun () -> repl ui st) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ()) 
