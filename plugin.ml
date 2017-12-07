(* Default plugin module *)

open File
open State
open Color
open CamomileLibrary


type command =
  | Find of string
  | Replace of string
  | Replace_All of string
  | Open_File of string
  | New_File of string
  | Change_Dir of string
  | Print_Dir

(* [parse_word s] returns the substring of [s] before the first [" "]. If
  * no [" "] occurs it returns [s] *)
let parse_word s =
  try
    let first_space = String.index s  ' ' in
    String.sub s 0 first_space
  with
  | Not_found -> s


let parse_command (s : string) : command option =
  let first_word = parse_word s in
  (* single word commands *)
  if first_word = s then
    match String.lowercase_ascii first_word with
    | "pwd" -> Some Print_Dir
    | _ -> None
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
    | "cd" -> Some (Change_Dir remainder)
    | _ -> None

(* [find_command st s_term] returns a copy of [st] with the next instance of
 * [s_term] selected *)
let find_command st s_term =
  let st' = s_term |> find st |> select_search_term in
  if get_selected_range st' = None
  then set_command_out st' (s_term^" not found")
  else set_command_out st' (s_term^" found")

(* [replace_command st terms] replaces the next instance of the first word in
 * [terms] with the second word in [terms]. If there is only one word it changes
 * the output to indicate to the user that replace requires 2 terms *)
let replace_command st terms =
  let s_term = parse_word terms in
  if s_term = terms
  then set_command_out st "replace requires 2 terms"
  else
    let remainder = String.(sub terms (length s_term+1)
                              (length terms - length s_term-1)) in
    let r_term = parse_word remainder |> String.trim in
    let st' = set_replace_term (find st s_term) r_term |> replace_next in
    if get_all_text st' = get_all_text st
    then set_command_out st' (s_term^" not found")
    else set_command_out st' ("\""^s_term^"\" replaced by \""^r_term^"\"")

(* [replace_all_command st terms] replaces the all instances of the first word in
 * [terms] with the second word in [terms]. If there is only one word it changes
 * the output to indicate to the user that replace_all requires 2 terms *)
let replace_all_command st terms =
  let s_term = parse_word terms in
  if s_term = terms
  then set_command_out st "replace_all requires 2 terms"
  else
    let remainder = String.(sub terms (length s_term+1)
                              (length terms - length s_term-1)) in
    let r_term = parse_word remainder |> String.trim in
    let st' = set_replace_term (find st s_term) r_term |> replace_all in
    if get_all_text st = get_all_text st'
    then set_command_out st' (s_term^" not found")
    else set_command_out st' ("all \""^s_term^"\" replaced by \""^r_term^"\"")

(* state after calling open command *)
let open_command st file_path =
  try
  let path = parse_word file_path in
  State.open_file st path
  with
  |Sys_error s -> set_command_out st s

(* state after calling new command *)
let new_file_command st file_path =
  let path = parse_word file_path in
  State.new_file st path;
  State.open_file st path

(* state after calling cd command *)
let cd_command st p =
  if change_directory p
  then set_command_out st (get_directory ())
  else set_command_out st ("Failed to move to " ^ p)

(* state after calling pwd command *)
let pwd_command st = set_command_out st (get_directory ())

let execute_command (cmd : command) (st : state) : state =
  match cmd with
  | Find s -> find_command st s
  | Replace s -> replace_command st s
  | Replace_All s ->replace_all_command st s
  | Open_File s ->open_command st s
  | New_File s ->new_file_command st s
  | Change_Dir s -> cd_command st s
  | Print_Dir -> pwd_command st

(* changes selection based on whether shift is pressed *)
let change_select shift st =
  match shift, (get_select_start st) with
  | true, None -> start_selecting st
  | false, Some _ -> unselect_text st
  | _ -> st

(* depending on whether there is selected text,
 * deletes selected text or calls a function on st *)
let delete_or_fun st f =
  match get_selected_range st with
  | None -> f st
  | Some (i0, i1) ->
    delete_text st i0 i1 |> unselect_text

let last_sep s c =
  match String.rindex_opt s c with
  | None -> s
  | Some ind -> String.sub s (ind+1) (String.length s - ind - 1)

let starts_with s b =
  String.(length s >= length b) && String.(sub s 0 (length b)) = b

let get_head = function
  | [] -> ""
  | h::t -> h

let tab_infer st =
  match get_command_in st with
  | None -> st
  | Some cmd_in ->
    let matches = ref [] in
    let full_path = last_sep cmd_in ' ' in
    let to_infer = last_sep full_path '/' in
    try
      if full_path <> to_infer
      (* there is some path before what is being inferred *)
      then
        let path = String.sub full_path 0 (String.length full_path - String.length to_infer) in
        let files = Sys.readdir (Filename.concat (get_directory()) path) in
        for n = 0 to (Array.length files - 1) do
          if (starts_with files.(n) to_infer)
          then matches := files.(n)::!matches
        done;
        match List.length !matches with
        | 0 -> st
        | _ -> let inferred = get_head !matches in
          set_command_in st (Filename.concat (String.sub cmd_in 0 ((String.length cmd_in) - (String.length to_infer))) (inferred))
      else
        (* there is no path before what is being inferred *)
        let files = Sys.readdir (get_directory()) in
        for n = 0 to (Array.length files - 1) do
          if (starts_with files.(n) to_infer)
          then matches := files.(n)::!matches
        done;
        match List.length !matches with
        | 0 -> st
        | _ ->
          let inferred = get_head !matches in
          set_command_in st ((String.sub cmd_in 0 ((String.length cmd_in) - (String.length to_infer)))^(inferred))
    with
    | Sys_error _ -> st

(* [press_key_file st k shift] computes new state when user presses [k]
 * while in a file. [shift] is whether the shift key is pressed. *)
let press_key_file st k shift = LTerm_key.( try
  match k with
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
          then st |> open_terminal
          else toggle_typing_area st
  | _ -> st
  with No_file_exn _ -> st)

(* [press_key_terminal st k shift] computes new state when user presses [k]
 * while in the terminal. [shift] is whether the shift key is pressed. *)
let press_key_terminal st k = LTerm_key.(
  match k with
  | Up -> cycle_up st
  | Down -> cycle_down st
  | Right -> cmd_cursor_right st
  | Left -> cmd_cursor_left st
  | Char c -> cmd_insert st (UChar.char_of c)
  | Backspace -> cmd_delete st
  | Delete ->
    if (st |> get_cmd_cursor) = (st |> cmd_cursor_right |> get_cmd_cursor)
    then st
    else st |> cmd_cursor_right |> cmd_delete
  | F2 ->
    begin
      match get_command_in st with
      | None -> open_terminal st
      | Some _ -> close_terminal st
    end
  | F3 -> toggle_typing_area st
  | Tab -> tab_infer st
  | _ -> st)

(* [ctrl_command st kc] returns the new state after the user presses
 * ctrl + [kc]. *)
let ctrl_command st kc = LTerm_key.(
  match kc with
  | Char z when (UChar.char_of z) = 'z' -> undo st
  | Char y when (UChar.char_of y) = 'y' -> redo st
  (* save file *)
  | Char s when (UChar.char_of s) = 's' ->
    State.save_file st (State.get_current_file st |> File.get_name)
  (* close file *)
  | Char w when (UChar.char_of w) = 'w' ->
    if is_on_file (State.close_file st) && get_typing_area st = File
    then State.close_file st
    else st |> State.close_file |> toggle_typing_area |> open_terminal
  (* | Tab ->
    *)
  | Char c when (UChar.char_of c) = 'c' -> copy st
  | Char v when (UChar.char_of v) = 'v' -> paste st
  | Char x when (UChar.char_of x) = 'x' -> cut st
  | Char w when (UChar.char_of w) = 'w' ->
    State.close_file st
  | Left -> State.tab_left st
  | Right -> State.tab_right st
  | Up -> if get_command_in st = None
          then st |> open_terminal
          else toggle_typing_area st
  | _ -> st)


let respond_to_event (event : LTerm_event.t) (st : state) : state =
  match event with
  | LTerm_event.Key{ control = true; code = keycode; _} ->
    if (is_on_file st) then ctrl_command st keycode else st
  | LTerm_event.Key { code = keycode; shift = shift; _ } ->
    begin
      match get_typing_area st with
      | File -> press_key_file st keycode shift
      | Command -> press_key_terminal st keycode
    end
  | _ -> st

let text_coloring (f : file) : color_mapping = failwith "Unimplemented"
