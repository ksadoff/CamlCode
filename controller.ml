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
  LTerm_ui.wait ui >>= function
  | LTerm_event.Key{ code = Escape; _ } ->
    return ()
  | LTerm_event.Key{ control = true; code = keycode; _} ->
    if (is_on_file !stref) then begin
      stref := match keycode with
        | Char z when (UChar.char_of z) = 'z' -> undo !stref
        | Char y when (UChar.char_of y) = 'y' -> redo !stref
        | _ -> !stref
        end;
        LTerm_ui.draw ui;
        repl ui stref

            (* | Char x when (UChar.int_of x) >=0 && (UChar.int_of x) < (num_open_files !stref) ->
          change_selected_file (List.nth (get_file_names !stref) (UChar.int_of x)) !stref
        | Char c when (UChar.char_of c) = 't' ->
          let old = get_current_file_name !stref in
                  let new = (List. *)
  | LTerm_event.Key { code = keycode; _ } ->
    if (is_on_file !stref) then begin
      stref := match keycode with
      | Right -> cursor_right !stref
      | Left -> cursor_left !stref
      | Up -> cursor_up !stref
      | Down -> cursor_down !stref
      | Char c -> insert_char !stref (UChar.char_of c)
      | Enter -> insert_char !stref '\n'
      | Backspace -> delete_char !stref
      | _ -> !stref
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
  let stref' = empty_state
    |> fun st -> open_file st "../../testtxts/somelines.txt" in

  let stref = open_file stref' "../../testtxts/easy.txt" |> fun x -> ref x in
  Lazy.force LTerm.stdout
  >>= fun term -> Clview.draw term stref
  >>= fun ui ->
    Lwt.finalize (fun () -> repl ui stref) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
