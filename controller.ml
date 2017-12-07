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
  match event, State.get_typing_area !stref with
  (* close program *)
  | LTerm_event.Key{ code = Escape; _ }, _ ->
    return ()
  (* enter command *)
  | LTerm_event.Key{ code = Enter; _ }, Command ->
    begin
      stref := match get_command_in !stref with
      | None -> failwith "unused"
      (* User presses enter to execute a command *)
      | Some cmd_str -> Plugin.parse_command cmd_str
        |> fun cmd_opt -> begin
          match cmd_opt with
          | Some cmd_in -> Plugin.execute_command cmd_in !stref |> update_commands
          | None -> set_command_out (!stref |> update_commands) "unrecognized command"
        end;
    end;
    LTerm_ui.draw ui;
    repl ui stref
  (* resize size of window *)
  | LTerm_event.Resize{ rows=rows; cols=cols }, _ -> 
    stref := set_total_height !stref rows;
    stref := set_width !stref cols;
    LTerm_ui.draw ui;
    repl ui stref
  (* for any other event, consult plugins *)
  | _ ->
    stref := Plugin.respond_to_event event !stref;
    LTerm_ui.draw ui;
    repl ui stref

(* Function to be called when starting the text editor with no files open.
 * The argument should be optional, when not given a string the
 * function is the same as if the file was not found *)
let main () =
  Unix.chdir "../..";

  let stref = ref empty_state in

  Lazy.force LTerm.stdout
  >>= fun term ->
    stref := set_total_height !stref ((LTerm.size term).rows);
    stref := set_width !stref (LTerm.size term).cols;
    Clview.draw term stref
  >>= fun ui ->
    Lwt.finalize (fun () -> repl ui stref) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
