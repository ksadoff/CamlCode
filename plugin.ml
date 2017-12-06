(* Default plugin module *)

open File
open State
open Color
open CamomileLibrary

(* Type of commands that can be input by the user. *)
type command = 
  | Find of string
  | Replace of string
  | Replace_All of string
  | Open_File of string
  | New_File of string
  
(* [parse_word s] returns the substring of [s] before the first [" "]. If
  * no [" "] occurs it returns [s] *)
let parse_word s =
  try
    let first_space = String.index s  ' ' in
    String.sub s 0 first_space
  with
  | Not_found -> s

(* [parse_command s] converts a raw input command [s] from the user 
 * to a command. Returns [None] if [s] is not a command for this plugin. *)
let parse_command (s : string) : command option = 
  let first_word = parse_word s in
  (* single word commands *)
  if first_word = s then None
  (* commands that take atleast one argument *)
  else
    let remainder = String.(sub s (length first_word+1)
                              (length s - length first_word-1)) in
    match String.lowercase_ascii first_word with
    | "find" -> Some (Find (parse_word remainder |> String.trim))
    | "replace" -> Some (Replace remainder)
    | "replace_all" -> Some (Replace_All remainder)
    | "open" -> Some(Open_File (remainder)) 
    | "new" -> Some (New_File (remainder))
    | _ -> None

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

let open_command st file_path =
  try
  let path = parse_word file_path in 
  State.open_file st path
  with 
  |Sys_error s -> set_command_out (set_command_in st "") s

let new_file_command st file_path = 
  let path = parse_word file_path in 
  State.new_file path;
  State.open_file st path

(* [execute_command cmd st] changes the state based on command [cmd]. *)
let execute_command (cmd : command) (st : state) : state = 
  match cmd with 
  | Find s -> find_command st s
  | Replace s -> replace_command st s
  | Replace_All s -> replace_all_command st s
  | Open_File s -> open_command st s
  | New_File s -> new_file_command st s

(* [respond_to_event event st] changes the state based on some event, 
 * such as a keyboard shorctut. *)
let respond_to_event (event : LTerm_event.t) (st : state) : state =
  match event with 
  | LTerm_event.Key{ control = true; code = keycode; _} ->
    if (is_on_file st) then begin
      match keycode with
        | Char z when (UChar.char_of z) = 'z' -> undo st
        | Char y when (UChar.char_of y) = 'y' -> redo st
        (* create new file *)
        | Char n when (UChar.char_of n) = 'n' -> 
          State.new_file "untitled";
          State.open_file st "untitled"
        (* save file *)
        | Char s when (UChar.char_of s) = 's' ->
          State.save_file st (State.get_current_file st |> File.get_name)
        (* close file *)
        (* known bug: when you close all the tabs and go to the welcome page,
         * you currently cannot type anything and none of the key bindings work.
         *)
        | Char w when (UChar.char_of w) = 'w' -> 
          State.close_file st
        (* | Tab -> 
           *)
        | _ -> st
        end
    else st

  | LTerm_event.Key { code = keycode; shift = shift; _ } ->
    if (is_on_file st) then begin
      match get_typing_area st with
      | File ->
        begin

          (* changes selection based on whether shift is pressed *)
          let change_select shift st =
            match shift, (get_select_start st) with 
            | true, None -> start_selecting st 
            | false, Some _ -> unselect_text st
            | _ -> st in

          (* depending on whether there is selected text,
          * deletes selected text or calls a function on st *)
          let delete_or_fun st f = 
            match get_selected_range st with 
            | None -> f st 
            | Some (i0, i1) -> 
              delete_text st i0 i1 |> unselect_text in

          match keycode with
          | Right -> st |> change_select shift |> cursor_right
          | Left -> st |> change_select shift |> cursor_left
          | Up -> st |> change_select shift |> cursor_up
          | Down -> st |> change_select shift |> cursor_down
          | Char c -> delete_or_fun st (fun x -> x)
            |> fun st -> insert_char st (UChar.char_of c)
          | Enter -> delete_or_fun st (fun x -> x)
            |> fun st -> insert_char st '\n'
          | Tab -> (* 1 tab = 4 spaces - can change w/ plugin *)
            delete_or_fun st (fun x -> x)
            |> fun st -> List.fold_left (fun st c -> insert_char st c) 
              st [' '; ' '; ' '; ' ']
          | Backspace -> delete_or_fun st delete_char
          | Delete -> delete_or_fun st 
            (fun st -> st |> cursor_right |> delete_char)
          | F2 ->
            begin
              match get_command_in st with
              | None -> open_terminal st
              | Some _ -> close_terminal st
            end
          | F3 -> if get_command_in st = None
                  then st
                  else toggle_typing_area st
          | _ -> st
        end

      | Command ->
        begin
          match keycode with
          | Right -> cmd_cursor_right st
          | Left -> cmd_cursor_left st
          | Char c -> cmd_insert st (UChar.char_of c)
          | Enter ->
            begin
              match get_command_in st with
              | None -> failwith "unused"
              (* User presses enter to execute a command *)
              | Some cmd_str -> parse_command cmd_str 
                |> fun cmd_opt -> begin
                  match cmd_opt with 
                  | Some cmd_in -> execute_command cmd_in st
                  | None -> st
                end 
            end
          | Backspace -> cmd_delete st
          | F2 ->
            begin
              match get_command_in st with
              | None -> open_terminal st
              | Some _ -> close_terminal st
            end
          | F3 -> toggle_typing_area st
          | _ -> st
        end
      end
  else st
  | _ -> st

(* [text_coloring f] creates a new coloring for file [f]. *)
let text_coloring (f : file) : color_mapping = failwith "Unimplemented"