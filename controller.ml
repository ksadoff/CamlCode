(* Controller Module *)
open Lwt
open LTerm_geom
open State
open Clview
open CamomileLibrary

(* [execute_command s flst st] finds the function in [flst] mapped to [s] and
 * returns the state returned by executing that function on [st]. If [s] does
 * not map to anything in [flst] returns [st] *)
let execute_command s flst st =
  try
    (flst |> List.assoc s) st
  with
  | Not_found -> st

(* returns the value [s] represented by [Some s], fails if the input is [None] *)
let extract = function
  | None -> failwith "not used"
  | Some s -> s

(* [repl ui stref] waits for input from the user. Once input is recieved it
 * creates a new state, updates the ui according to the new state
 * (by calling update in CLView) and passes the
 * new state to another call of [repl] recursively *)
let rec repl ui stref =
  LTerm_ui.wait ui >>= function
  | LTerm_event.Key{ code = Escape; _ } ->
    return ()
  | LTerm_event.Key { code = keycode; _ } ->
    if (is_on_file !stref) then begin
      match get_typing_area !stref with
      | File ->
        begin
          stref := match keycode with
          | Right -> cursor_right !stref
          | Left -> cursor_left !stref
          | Up -> cursor_up !stref
          | Down -> cursor_down !stref
          | Char c -> insert_char !stref (UChar.char_of c)
          | Enter -> insert_char !stref '\n'
          | Backspace -> delete_char !stref
          | F2 ->
            begin
              match get_command_in !stref with
              | None -> open_terminal !stref
              | Some _ -> close_terminal !stref
            end
          | F3 -> toggle_typing_area !stref
          | _ -> !stref
        end
      | Command ->
        begin
          stref := match keycode with
          | Char c -> set_command_in !stref
                        ((!stref |> get_command_in |> extract)^
                         (c |> UChar.char_of |> Char.escaped))
          | Backspace ->
            begin
              let cmd_in = !stref |> get_command_in |> extract in
              if cmd_in = "" then !stref
              else set_command_in !stref String.(sub cmd_in 0 (length cmd_in-1))
            end
          | F2 ->
            begin
              match get_command_in !stref with
              | None -> open_terminal !stref
              | Some _ -> close_terminal !stref
            end
          | F3 -> toggle_typing_area !stref
          | _ -> !stref
        end
    end;
    LTerm_ui.draw ui;
    repl ui stref
  | _ -> repl ui stref

(* Function to be called when starting the text editor with no files open.
 * The argument should be optional, when not given a string the
 * function is the same as if the file was not found *)
let main () =
  (* TODO: Right now, a somelines.txt is automatically opened.
   * We need to open [empty_state] by default instead, and then have
   * the user choose the file with a command. *)
  let stref = empty_state
    |> fun st -> open_file st "../../testtxts/somelines.txt"
    |> fun x -> ref x in
  Lazy.force LTerm.stdout
  >>= fun term -> Clview.draw term stref
  >>= fun ui ->
    Lwt.finalize (fun () -> repl ui stref) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
