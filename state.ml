(* The State module contains the entire state of the program,
 * including a list of all files being used. *)

open Color
open File
open Filename

(* Raised when calling a function that requires an open file
 * without an open file. *)
exception No_file_exn of string

(* Indicates whether or not a file is open *)
type opened_file = Nofile | Fname of string

type typing_area = Command | File

type clipboard = Rope.t

type size = LTerm_geom.size

(* State of the program. Contains the following information:
 * * List of files currently open
 * * List of files displayed on screen (split screen)
 * * The typing area that is currently being edited
 * * List of most recently used commands
 * * Clipboard for copy/paste
 * * Name of the current file
 * * First (top) visible line of text
 * * Start and end locations for a block of selected text
 * * Current search term *)
type state = {
  (* associative list mapping file name to file *)
  files: (string * File.file) list;
  (* list of file names open on split screen *)
  screens: string list;
  (* currently open file *)
  current_file: opened_file;
  (* Indicates whether or not a file is open *)
  typing_loc : typing_area;
  (* clipboard *)
  clipboard: clipboard;
  (* text to be displayed to the user in the command prompt, [None] if
   * the terminal is not open *)
  command_out : string option;
  (* text input to the command prompt from the user, [None] if the terminal
   * is not open *)
  command_in : string option;
  (* indictes the position of the cursor in the command prompt *)
  command_cursor : int;
  (* command history *)
  up_cmds : string list;
  down_cmds : string list;
  (* the height of the terminal *)
  total_height : int;
  (* the height of the window we want *)
  height : int;
  (* the width of the terminal window *)
  width : int;

}

let max_cmds = 50

(* HELPER FUNCTIONS *)

let set_total_height st h =
  {st with total_height = h; height = h}

let set_width st w =
  {st with width = w}

let get_width st =
  st.width

let get_height st =
  st.height

(* [rem_tail lst] returns a copy of [lst] with the last element removed *)
let rec rem_tail = function
  | []
  | _::[] -> []
  | h::t -> h::(rem_tail t)

(* [get_current_file st] returns the file that is currently being manipulated.
 * Raises [Invalid_argument] if no file currently selected and [Not_found]
 * if [current_file] holds a string that is not a file being used. *)
let get_current_file st =
  match st.current_file with
  | Fname s -> List.assoc s st.files
  | _ -> raise (No_file_exn "no file selected")

(* [find_index lst x acc] Given an element and a list and an accumulator, this
 * function returns the index of that element in the first occurrance of the
 * element in the list. This function should only be called on lists where we
 * know the element we are looking for exists.
 *)
let rec find_index lst x acc =
  match lst with
  |[] -> raise (No_file_exn "Unused - find_index")
  |h::t -> if h = x then acc else find_index t x (acc+1)

(* [is_on_file st] returns [true] if there user is currently on a file,
 * and [false] if the user does not have a file open or if they
 * are typing on the command prompt. *)
let is_on_file st =
  match st.current_file with
  | Fname _ -> true
  | _ -> false

(* [file_to_state_fun f_fun st] takes a function that acts on a file
 * [f_fun : file -> 'a] and returns a function of type [state -> 'a]
 * that calls to [f_fun] but uses the current file in [st] as input.
 * Raises [Invalid_argument] if no file currently selected and [Not_found]
 * if [current_file] holds a string that is not a file being used. *)
let file_to_state_fun f_fun st =
  match st.current_file with
  | Fname s -> f_fun (get_current_file st)
  | _ -> raise (No_file_exn "no file selected")

(* [fmap_st_f f_fun st] takes a function [f_fun : file -> file],
 * executes it on the currently selected file in [st] to get [f'],
 * and returns a new state with [f'] replacing [f].
 * If [st] not current on a file, returns [st]. *)
let fmap_st_f f_fun st =
  let f' = file_to_state_fun f_fun st in
  let s = File.get_name f' in
  { st with files = (s, f') :: (List.remove_assoc s st.files) }

(* [replace_current_file st f] replaces the current file in [st] with [f]
 * and searches through the list of files in [st] and replaces the
 * the instance with [f]'s name in the list. *)
let replace_current_file st f =
  let file_name = File.get_name f in
  { st with
    files = begin
      let fname = file_name in
      (fname, f) :: (List.remove_assoc fname st.files)
    end;
    current_file = Fname file_name;
  }

(* TYPING AREA *)

(* [get_file_names st] returns a list of strings that represent the names of
 * the currently open files. *)
let get_file_names st =
  List.map (fun x -> fst x) st.files

(* [num_open_files st] is number of open files in [st] *)
let num_open_files st = List.length st.files

(* [get_current_file_name st] returns the string of the name of the file being
 * manipulated. *)
let get_current_file_name st =
  match st.current_file with
  | Fname s -> s
  | _ -> raise (No_file_exn "no file selected")

(* [set_current_file st f] sets the current file in [st] to [f]. *)
let set_current_file st f = {st with current_file = Fname (get_name f)}

(* [change_selected_file s st] changes the selected file in [st]
  * to the file with name [s].
  * Raises Not_found if [s] is not one of the files open in [st]. *)
let change_selected_file s st =
  {st with current_file = Fname s }

(* [tab_right st] takes in a state and returns a state with the current file
 * being replaced with the file that appears next in the list of open files.
 * If the current file is the last file in the list,
 * then it will return the current file. *)
let tab_right st =
  let file_names = List.map (fun x -> fst x) st.files in
  match st.current_file with
  | Fname curr_fname -> begin
    let right_file_index = (find_index file_names curr_fname 0) + 1 in
    if (right_file_index >= List.length file_names)
    then st
    else {st with current_file = Fname (List.nth file_names right_file_index)}
  end
  | Nofile -> st

(* [tab_left st] takes in a state and returns a state with the current file
 * being replaced with the file that appears previous in the list of open files.
 * If the current file is the first file in the list,
 * then it will return the current file. *)
let tab_left st =
  let file_names = List.map (fun x -> fst x) st.files in
  match st.current_file with
  | Fname curr_fname -> begin
    let curr_file_index = (find_index file_names curr_fname 0) in
    let left_file_index = curr_file_index - 1 in
    if (curr_file_index <= 0)
    then st
    else {st with current_file = Fname (List.nth file_names left_file_index)}
  end
  | Nofile -> st

(* [get_typing_area st] returns the typing area of [st], either the command
 * prompt or a file *)
let get_typing_area st = st.typing_loc


(* [toggle_typing_area st] returns a copy of [st] with its typing area swapped *)
let toggle_typing_area st =
  match st.typing_loc with
  | Command -> { st with typing_loc = File; }
  | File -> { st with typing_loc = Command; }

(* CURRENT DIRECTORY *)

(* [change_directory d] changes the current directory of this program
 * to [d] in the way that Unix would. Returns whether the move worked. *)
let change_directory d = try Unix.chdir d; true
  with Unix.Unix_error _ -> false

(* [get_directory ())] is the current directory in [st]. *)
let get_directory = Sys.getcwd

(* FILE OPERATIONS *)

(* [convert_path st p] returns the string filepath [p] appended
 * to the current working directory if it is a relative path. *)
let convert_path p =
  if is_relative p
  then concat (get_directory ()) p
  else p

(* [new_file st s] creates a new, empty file with name [s], relative
 * to the current working directory of [st].
 * Raises [Sys_error] if creating file failed. *)
let new_file st s =
  let p = convert_path s in
  let ch_out = open_out p in
  close_out ch_out

(* [open_file st s] constructs the file with name [s] and adds it
 * to the list of files in state [st].
 * Raises [Sys_error] if file read failed. *)
let open_file st s =
  let p = convert_path s in
  let file_names = List.map (fun x -> fst x) st.files in
  if (List.exists (fun x -> x = p) file_names) then
    {st with current_file = Fname p}
  else
  let new_file = File.open_file p in
  { st with
    files = (p, new_file) :: st.files;
    screens = [];
    current_file = Fname p;
  }

(* [save_file st s] saves the currently selected file in [st]
 * at relative path [s].
 * Raises [Sys_error] if file write failed. *)
let save_file st s =
  let p = convert_path s in
  fmap_st_f (fun f -> File.save_file f p) st

(* [is_filed_saved st s] returns the file named [s] in state [st] is saved.
 * Raises [Not_found] if file does not exist in [st]. *)
let is_file_saved st s =
  List.assoc s st.files |> fun f -> File.is_saved f

(* [close_file st] removes the currently selected file [f]
 * from the list of open files in [st]. The newly selected file
 * becomes the file at the beginning of the list of files in [st].
 * If no file is currently selected, returns [st]. *)
let close_file st =
  match st.current_file with
  | Fname s ->
    let newfiles = List.remove_assoc s st.files in {
      st with
      files = newfiles;
      screens = List.filter (fun x -> x <> s) st.screens;
      current_file = begin
        match newfiles with
        | [] -> Nofile
        | (s,_)::_ -> Fname s
      end;
    }
  | _ -> st

(* CLIPBOARD *)

let new_clipboard = Rope.empty

let string_to_clipboard s = Rope.of_string s

let clipboard_to_string st = Rope.to_string st.clipboard

(* [get_clipboard st] returns the current clipboard of st *)
let get_clipboard st = st.clipboard

(* [copy st] returns a copy of state with the text selected in the open file of
 * [st] saved to the clipboard *)
let copy st =
  let curr = get_current_file st in
  match (get_selected_range curr) with
  | None -> st
  | Some (loc1, loc2) ->
    let new_clipboard = (File.get_text curr loc1 loc2 |> Rope.of_string) in
  {st with clipboard = new_clipboard}

(* [paste st] returns a copy of state with the text from the clipboard of [st]
 * inserted at the cursor location in the open flie of [st] *)
let paste st =
  let curr = get_current_file st in
  let new_file = File.insert_text curr (Rope.to_string st.clipboard) (File.get_cursor_location curr) in
  replace_current_file st new_file

let cut st =
  let curr = get_current_file st in
  let new_st = copy st in
  match (get_selected_range curr) with
  | None -> st
  | Some (loc1, loc2) -> let new_file = delete_text curr loc1 loc2 in
    replace_current_file new_st new_file

(* COMMAND PROMPT *)

(* [add_up_cmds st] returns the previous commands queue of [st] with its
 * current command input pushed, if it not [None] *)
let add_up_cmds st =
  match st.command_in with
  | None -> st.down_cmds
  | Some cmd_in ->
    if List.length st.up_cmds < max_cmds
    then cmd_in::st.up_cmds
    else  cmd_in::(rem_tail st.up_cmds)

(* [add_down_cmds st] returns the previous commands down queue of [st] with its
 * current command input pushed, if it not [None] *)
let add_down_cmds st =
  match st.command_in with
  | None -> st.up_cmds
  | Some cmd_in ->
    if List.length st.down_cmds < max_cmds
    then cmd_in::st.down_cmds
    else  cmd_in::(rem_tail st.down_cmds)

(* [cycle_up st] returns a copy of [st] with the command input set to the next
 * command in the stack of previously used commands *)
let cycle_up st =
  match st.up_cmds with
  | [] -> st
  | h::t -> { st with
              command_in = Some h;
              command_cursor = String.length h;
              up_cmds = t;
              down_cmds = add_down_cmds st  }

(* [cycle_down st] returns a copy of [st] with the command input set to the next
 * command in the stack of things popped from the previously used commands*)
let cycle_down st =
  match st.down_cmds with
  | [] -> st
  | h::t -> { st with
              command_in = Some h;
              command_cursor = String.length h;
              up_cmds = add_up_cmds st;
              down_cmds = t }

(* [update_commands st] returns [st], with the command input set to empty,
 * the previous commant input of [st] pushed to the previous command stack, and
 * the down command stack cleared *)
let update_commands st =
  { st with command_in = Some "";
            command_cursor = 0;
            up_cmds = add_up_cmds st;
            down_cmds = [];
  }


(* [get_command_out st] returns the [command_out] field of [st] *)
let get_command_in st = st.command_in

(* [open_terminal st] returns a copy of [st] with both [command_out] and
 * [command_in] set to [Some ""] if they are [None] in [st] which indicates
 * that the terminal is open but no text is displayed. If the terminal is open
 * in [st] it returns [st] *)
let open_terminal st =
  match st.command_out with
  | None -> { st with typing_loc = Command;
                      command_out = Some "";
                      command_in = Some "";
                      command_cursor = 0;
                      height = st.total_height - 8}
                        (* let tabheight =
                  match get_command_in st with
                  | Some _ -> 8
                  | None -> 5 in st.height - 8} *)
  | Some _ -> st

(* [close_terminal st] returns a copy of [st] with both [command_out] and
 * and [command_in] both set to [None], indicating that the terminal is closed *)
let close_terminal st = { st with typing_loc = File;
                                  command_out = None;
                                  command_in = None;
                        height = st.total_height - 5}

(* [set_command_out st s] returns a copy of [st] with [command_out] set to
 * [Some s], if the terminal is not open in [st] the returned value also has
 * [command_in] set to [Some ""] *)
let set_command_out st s =
  match st.command_in with
  | None -> { st with command_out = Some s; command_in = Some "";}
  | Some _ -> { st with command_out = Some s; }

(* [get_command_out st] returns the [command_out] field of [st] *)
let get_command_out st = st.command_out

(* [set_command_in st s] returns a copy of [st] with [command_in] set to
 * [Some s], if the terminal is not open in [st] the returned value also has
 * [command_out] set to [Some ""] *)
let set_command_in st s =
  match st.command_out with
  | None -> { st with command_cursor = 0; command_out = Some ""; command_in = Some s;}
  | Some _ -> { st with command_cursor = String.length s; command_in = Some s; }

(* [cmd_insert st c] returns a copy of [st] with [c] inserted at the command
 * cursor location in the command input and the cursor moved one space right *)
let cmd_insert st c =
  match st.command_in with
  | Some cmd_in ->
    let new_cmd_in = String.((sub cmd_in 0 st.command_cursor)^
                             (c |> Char.escaped)^
                             (sub cmd_in st.command_cursor
                                ((length cmd_in)-st.command_cursor))) in
    { st with command_in = Some (new_cmd_in);
              command_cursor = st.command_cursor + 1; }
  | None -> { st with command_in = Some (Char.escaped c);
                      command_cursor = 1; }

(* [cmd_delete st c] returns a copy of [st] with the character at the location
 * of the command cursor in the command input deleted and the command cursor moved
 * one space left *)
let cmd_delete st =
  match st.command_in with
  | None -> st
  | Some cmd_in -> if st.command_cursor > 0
                  then{ st with
                        command_in = Some String.((sub cmd_in 0 (st.command_cursor-1))^
                                                  (sub cmd_in (st.command_cursor)
                                                 ((length cmd_in)-st.command_cursor)));
                        command_cursor = st.command_cursor - 1;
                      }
                  else st

(* [get_cmd_cursor st] returns the location of the cursor in the command prompt *)
let get_cmd_cursor st = st.command_cursor

(* [cmd_cursor_right st] returns a copy of [st] with the command cursor moved
 * one space to the right *)
let cmd_cursor_right st =
  match st.command_in with
  | None -> st
  | Some cmd_in ->
    if st.command_cursor < (String.length cmd_in) then
      {st with command_cursor = st.command_cursor + 1; }
    else st

(* [cmd_cursor_left st] returns a copy of [st] with the command cursor moved
 * one space to the left *)
let cmd_cursor_left st =
match st.command_in with
  | None -> st
  | Some cmd_in ->
    if st.command_cursor > 0 then
      {st with command_cursor = st.command_cursor - 1; }
    else st

let get_cmd_text st =
  match st.command_in with
  | None -> failwith "not used"
  | Some "" -> " "
  | Some s -> if st.command_cursor < String.length s
              then String.(sub s st.command_cursor 1)
              else " "


(* CURSOR *)

(* [get_cursor_location st] gets the location of the cursor in the file open
 * in [st]. *)
let get_cursor_location = file_to_state_fun File.get_cursor_location

(* [get_cursor_line_num st] returns the line number of the cursor in
 * the file that is currently open in [st]. *)
let get_cursor_line_num = file_to_state_fun File.get_cursor_line_num

(* [get_cursor_line_num st] returns the column of the cursor in
 * the file that is currently open in [st]. *)
let get_cursor_column = file_to_state_fun File.get_cursor_column

(* [move_cursor st l] moves the cursor of the open file in [st] to [l] *)
let move_cursor st l = fmap_st_f (fun f -> File.move_cursor f l) st

(* [cursor_left st] moves the cursor left on the currently selected
 * file in [st]. *)
let cursor_left st =
  fmap_st_f (fun f -> File.cursor_left_scroll f st.width st.height) st

(* [cursor_right st] moves the cursor right on the currently selected
 * file in [st]. *)
let cursor_right st =
  fmap_st_f (fun f -> File.cursor_right_scroll f st.width st.height) st

(* [cursor_up st] moves the cursor up on the currently selected file
 * in [st]. *)
let cursor_up st = fmap_st_f
    (fun f -> File.cursor_up_scroll f st.width st.height) st

(* [cursor_down st] moves the cursor down on the currently selected file
 * in [st]. *)
let cursor_down st =
  fmap_st_f (fun f -> File.cursor_down_scroll f st.width st.height) st

(* SCROLLING *)

(* [scroll_to st n] changes the line number of the scrolled view of
 * the file open in [st] to to [n]. *)
let scroll_to st n = fmap_st_f (fun f -> File.scroll_to f n) st

(* [get_scroll_line st] returns the first visible line in the
 * currently selected file in [st]. *)
let get_scroll_line = file_to_state_fun File.get_scroll_line

(* [get_scrolled_lines st] displays the currently scrolled to lines,
 * so that the cursor is viewable horizontally and the first line displayed
 * is the current scroll line. *)
let get_scrolled_lines st =
  file_to_state_fun (fun f -> File.get_scrolled_lines f st.width (st.height+1)) st

(* READ TEXT *)

(* [get_text st l1 l2] returns all text in the open file of [st] from
 * [l1] to [l2]. Raises Invalid_argument if [l2] comes before [l1]. *)
let get_text = file_to_state_fun File.get_text

(* [get_all_text st] returns a string representing all of the text in
 * the file opened in [st] *)
let get_all_text = file_to_state_fun File.get_all_text

let get_visible_text st numlines =
  let curr = get_current_file st in
  File.get_visible_text curr numlines

(* SELECTING TEXT *)

(* [start_selecting st] sets the fixed selecting point to the current
 * location of the cursor in the currently selected file in [st]. *)
let start_selecting = fmap_st_f File.start_selecting

(* [select_text st l1 l2] selects text from [l1] to [l2] in the currently
 * selected file in [st]. This function forces [l1] and [l2] to be in order
 * and in bounds. *)
let select_text st l1 l2 = fmap_st_f (fun f -> File.select_text f l1 l2) st

(* Returns [st] with no selected text in its current file. *)
let unselect_text = fmap_st_f File.unselect_text

(* [get_selected_range f] returns [None] if no text is selected,
 * or [Some (i1, i2)] if there is currently text selected from
 * index [i1] to [i2]. *)
let get_selected_range = file_to_state_fun File.get_selected_range

(* [get_select_start f] returns [Some (i, l, c)] where [i]
 * is the index of the beginning of the selection region, [l] is the line
 * number, and [c] is the column. If not selection has been made,
 * returns None. *)
let get_select_start = file_to_state_fun File.get_select_start

(* EDIT TEXT *)

(* [insert_text st s l] inserts string [s] into the contents the open
 * file of [st] at location [l]. *)
let insert_text st s l = fmap_st_f (fun f -> File.insert_text f s l) st

(* [insert_char st c] inserts a character [c] at the cursor position
 * in the currently selected file in [f] and moves the cursor one
 * position to the right. *)
let insert_char st c =
  fmap_st_f (fun f -> File.insert_scroll f c st.width st.height) st

(* [delete_text st l1 l2] deletes all the text in the currently held
 * file from location [l1] to [l2]. *)
let delete_text st l1 l2 = fmap_st_f (fun f -> File.delete_text f l1 l2) st

(* [delete_char st] deletes the character before the cursor postion
 * in the currently selected file in [st] and moves the cursor
 * to the left accordingly. *)
let delete_char st =
  fmap_st_f (fun f -> File.delete_scroll f st.width st.height) st

(* UNDO/REDO *)

(* [undo st] undoes the last change recorded in the open file of [st].
 * If there is nothing left to undo, [undo st] will return [st] unchanged. *)
let undo st = fmap_st_f File.undo st

(* [redo st] redoes the last change that was undone in the open file of
 * [st]. If there is nothing left to redo, [redo st] will return [st]
 * unchanged. *)
let redo st = fmap_st_f File.redo st

(* COLORING *)

(* [color_text st lst] returns a copy of [st] with the open file now
 * having the color mappings of [lst] *)
let color_text st lst = {st with current_file = Fname (File.get_name (File.color_text (get_current_file st) lst))}

(* [get_coloring st] gets the color mapping of the currently
 * open file in [st]. *)
let get_coloring st = File.get_coloring (get_current_file st)

(* FIND AND REPLACE *)

(* [get_search_term st] gets the current search term in [st]. *)
let get_search_term st = File.get_search_term (get_current_file st)

(* [select_search_term st] returns an updated version of [st] with the currently selected file
 * with the next instance of the search term selected. The next instance is
 * defined as from the currently selected text. If no text is selected, the
 * new version of the selected file will have the first instance of its search term selected.
 * If there is no search term or it is not found, returns [st] with the selected file no text
 * selected *)
let select_search_term st = fmap_st_f File.select_search_term st

(* [find st s] updates [st] so that it holds [s] as its current
 * search term in its currently selected file. *)
let find st s = fmap_st_f (fun f -> File.find f s) st

(* [remove_search_term st] removes the search_term of file currently selected
 * in [st] *)
let remove_search_term st = fmap_st_f File.remove_search_term st

(* [set_replace_term st s] sets the replace term of file opened in [st] to
 *  to [Some s] unless s = "" or "\n" *)
let set_replace_term st s = fmap_st_f (fun f -> File.set_replace_term f s) st

(* [remove_replace_term st] sets the replace term of file opened in [st] to [None] *)
let remove_replace_term st = fmap_st_f File.remove_replace_term st

(* [get_replace_term f] returns [Some s] where [r] is the replacement term
 * if the is no replacement term returns [None] *)
let get_replace_term st = File.get_replace_term (get_current_file st)

(* [replace_next st] calls [File.replace_next f] where [f] is the currently
 * selected file in [st] and changes the currectly selected file to be the
 * the returned file *)
let replace_next st = fmap_st_f File.replace_next st

(* [replace_all st] calls [File.replace_all f] where [f] is the currently
 * selected file in [st] and changes the currectly selected file to be the
 * the returned file *)
let replace_all st = fmap_st_f File.replace_all st

(* this is at the end because it requires other things *)

(* New state with no files open yet *)
let empty_state =
  {
    files = [];
    screens = [];
    current_file = Nofile;
    typing_loc = Command;
    clipboard = new_clipboard;
    command_out = Some "";
    command_in = Some "";
    command_cursor = 0;
    up_cmds = [];
    down_cmds = [];
    total_height = 0;
    height = 0;
    width = 0
  }
let first_index_of_line st linenum =
  File.first_index_of_line (get_current_file st) linenum

let last_index_of_line st linenum =
  File.last_index_of_line (get_current_file st) linenum

let get_visible_text st numlines =
  let curr = get_current_file st in
  File.get_visible_text curr numlines
