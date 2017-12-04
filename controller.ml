(* Controller Module *)
open Lwt
open LTerm_geom
open State
open Clview
open CamomileLibrary

(* type to represent valid command prompt commands *)
type commands =
  | Find
  | Replace
  | Replace_All
  | Invalid


(* [parse_word s] returns the substring of [s] before the first [" "]. If
 * no [" "] occurs it returns [s] *)
let parse_word s =
  try
    let first_space = String.index s  ' ' in
    String.sub s 0 first_space
  with
  | Not_found -> s

(* [string_to_command s] parses a [s] into a intance of type command *)
let string_to_command s =
  let first_word = parse_word s in
  (* single word commands *)
  if first_word = s then
    Invalid, ""
  (* commands that take atleast one argument *)
  else
    let remainder = String.(sub s (length first_word+1)
                              (length s - length first_word-1)) in
    match String.lowercase_ascii first_word with
    | "find" -> Find, (parse_word remainder |> String.trim)
    | "replace" -> Replace, (remainder)
    | "replace_all" -> Replace_All, (remainder)
    | _ -> Invalid, ""

(* [execute_command s flst st] finds the function in [flst] mapped to [s] and
 * returns the state returned by executing that function on [st] with the command
 * input now empty. If [s] does not map to anything in [flst] returns [st] *)
let execute_command s flst st =
  try
    let c = string_to_command s in
    let f = List.assoc (fst c) flst in
    let st' = set_command_out (set_command_in st "") "" in
    f st' (snd c)
  with
  | Not_found -> set_command_out (set_command_in st "") "unrecognized command"

(* [find_command st s_term] returns a copy of [st] with the next instance of
 * [s_term] selected *)
let find_command st s_term =
  let st' = s_term |> find st |> select_search_term in
  if get_selected_range st = get_selected_range st'
  then set_command_out st' (s_term^" not found")
  else st'

(* [replace_command st terms] replaces the next instance of the first word in
 * [terms] with the second word in [terms]. If there is only one word it changes
 * the output to indicate to the user that replace requires 2 terms *)
let replace_command st terms =
  let s_term = parse_word terms in
  if s_term = terms
  then set_command_out (set_command_in st "") "replace requires 2 terms"
  else
    let remainder = String.(sub terms (length s_term+1)
                              (length terms - length s_term-1)) in
    let r_term = parse_word remainder |> String.trim in
    let st' = set_replace_term (find st s_term) r_term |> replace_next in
    if get_all_text st = get_all_text st'
    then set_command_out st' (s_term^" not found")
    else st'

  (* [replace_command st terms] replaces the next instance of the first word in
   * [terms] with the second word in [terms]. If there is only one word it changes
   * the output to indicate to the user that replace requires 2 terms *)
  let replace_all_command st terms =
    let s_term = parse_word terms in
    if s_term = terms
    then set_command_out (set_command_in st "") "replace requires 2 terms"
    else
      let remainder = String.(sub terms (length s_term+1)
                                (length terms - length s_term-1)) in
      let r_term = parse_word remainder |> String.trim in
      let st' = set_replace_term (find st s_term) r_term |> replace_all in
      if get_all_text st = get_all_text st'
      then set_command_out st' (s_term^" not found")
      else st'

(* a mapping of [Command]'s to functions for the command prompt *)
let flst = [
  (Find, find_command);
  (Replace, replace_command);
  (Replace_All, replace_all_command)
]

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
          | F3 -> if get_command_in !stref = None
                  then !stref
                  else toggle_typing_area !stref
          | _ -> !stref
        end
      | Command ->
        begin
          stref := match keycode with
          | Right -> cmd_cursor_right !stref
          | Left -> cmd_cursor_left !stref
          | Char c -> cmd_insert !stref (UChar.char_of c)
          | Enter ->
            begin
              match get_command_in !stref with
              | None -> failwith "unused"
              | Some cmd_in -> execute_command cmd_in flst !stref
            end
          | Backspace -> cmd_delete !stref
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
  let stref' = empty_state
    |> fun st -> open_file st "../../testtxts/somelines.txt" in

  let stref = open_file stref' "../../testtxts/easy.txt" |> fun x -> ref x in
  Lazy.force LTerm.stdout
  >>= fun term -> Clview.draw term stref
  >>= fun ui ->
    Lwt.finalize (fun () -> repl ui stref) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ())
