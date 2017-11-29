(* The State module contains the entire state of the program,
 * including a list of all files being used. *)

open Color

(* Represents the area where user is typing, i.e. in a file or
 * in the command line. *)
type typing_area = Nofile | Command | Fname of string

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
  (* associative list mapping file name to file, used for determining which
   files will appear for split screen *)
  screens: string list;
  (* currently open file *)
  current_file: typing_area;
}

(* [extract file_opt] takes in an 'a option and returns the 'a. *)
let extract file_opt = 
  match file_opt with
  | Some f -> f
  | None -> failwith "Unused"
 
(* [get_current_file st] returns the file that is currently being manipulated.
 * Raises [Invalid_argument] if no file currently selected and [Not_found] 
 * if [st.current_file] holds a string that is not a file being used. *)
let get_current_file st = 
  match st.current_file with 
  | Fname s -> List.assoc s st.files
  | _ -> raise (Invalid_argument "no file selected")

(* [file_to_state_fun f_fun st] takes a function that acts on a file
 * [f_fun : file -> 'a] and returns a function of type [state -> 'a]
 * that calls to [f_fun] but uses the current file in [st] as input.
 * Raises [Invalid_argument] if no file currently selected and [Not_found] 
 * if [st.current_file] holds a string that is not a file being used. *)
let file_to_state_fun f_fun st = 
  match st.current_file with 
  | Fname s -> f_fun (get_current_file st)
  | _ -> raise (Invalid_argument "no file selected")

(* [fmap_st_f f_fun st] takes a function [f_fun : file -> file],
 * executes it on the currently selected file in [st] to get [f'], 
 * and returns a new state with [f'] replacing [f]. *)
let fmap_st_f f_fun st = 
  let f' = file_to_state_fun f_fun st in
  let s = File.get_name f' in
  { st with files = (s, f') :: (List.remove_assoc s st.files) }

(* [replace_current_file st f] replaces the current file in [st] with [f]
 * and searches through the list of files in [st] and replaces the 
 * the instance with [f]'s name in the list. *)
let replace_current_file st f = 
  let file_name = File.get_name f in
  {
    files = begin
      let fname = file_name in 
      (fname, f) :: (List.remove_assoc fname st.files)
    end;
    screens = st.screens;
    current_file = Fname file_name;
  }

(* [new_file s] creates a new, empty file at path [s].
 * Raises [Sys_error] if creating file failed. *)
let new_file s = let ch_out = open_out s in close_out ch_out

(* New state with no files open yet *)
let empty_state = 
  {
    files = [];
    screens = [];
    current_file = Nofile;
  }

(* [get_file_names st] returns a list of strings that represent the names of
 * the currently open files. *)
let get_file_names st = 
  List.map (fun x -> fst x) st.files

 (* [get_current_file_name st] returns the string of the name of the file being 
  * manipulated. *)
let get_current_file_name st = 
    let f = get_current_file st in
    File.get_name f

(* [open_file st s] constructs the file at path [s] and adds it
 * to the list of files in state [st].
 * Raises Sys_error if file read failed. *)
let open_file st s = 
  let new_file = File.open_file s in 
  {
    files = (s, new_file) :: st.files;
    screens = [];
    current_file = Fname s;
  }

(* [is_filed_saved st] returns true if the file is saved and false if not*)
let is_file_saved st = failwith "Unimplemented"

(* [save_file st] saves the currently selected file in [st] at
 * its corresponding path.
 * Raises Sys_error if file write failed. *)
let save_file = file_to_state_fun File.save_file

(* [close_file st] removes the currently selected file [f]
 * from the list of open files in [st]. The newly selected file
 * becomes the file at the beginning of the list of files in [st].
 * If no file is currently selected, returns [st]. *)
let close_file st = 
  match st.current_file with 
  | Fname s -> 
    let newfiles = List.remove_assoc s st.files in { 
      files = newfiles;
      screens = List.filter (fun x -> x <> s) st.screens;
      current_file = begin 
        match newfiles with 
        | [] -> Nofile
        | (s,_)::_ -> Fname s
      end;
    }
  | _ -> st

(* [change_selected_file s st] changes the selected file in [st]
 * to the file with name [s].
 * Raises Not_found if [s] is not one of the files open in [st]. *)
let change_selected_file s st = 
  {st with current_file = Fname s }

(* [copy st] returns a copy of state with the text selected in the open file of
 * [st] saved to the clipboard *)
let copy st = failwith "Unimplemented"

(* [paste st] returns a copy of state with the text from the clipboard of [st]
 * inserted at the cursor location in the open flie of [st] *)
let paste st = failwith "Unimplemented"

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
let cursor_left = fmap_st_f File.cursor_left
 
(* [cursor_right st] moves the cursor right on the currently selected
 * file in [st]. *)
let cursor_right = fmap_st_f File.cursor_right
 
(* [cursor_up st] moves the cursor up on the currently selected file
 * in [st]. *)
let cursor_up = fmap_st_f File.cursor_up

(* [cursor_down st] moves the cursor down on the currently selected file
 * in [st]. *)
let cursor_down = fmap_st_f File.cursor_down

(* [scroll_to st n] changes the line number of the scrolled view of
 * the file open in [st] to to [n]. *)
let scroll_to st n = failwith "Unimplemented"

(* [get_scroll_line_number st] returns the first visible line in the
 * currently selected file in [st]. *)
let get_scroll_line_number st = failwith "Unimplemented"

(* [get_text st l1 l2] returns all text in the open file of [st] from
 * [l1] to [l2]. Raises Invalid_argument if [l2] comes before [l1].  *)
let get_text = file_to_state_fun File.get_text

(* [get_all_text st] returns a string representing all of the text in
 * the file opened in [st] *)
let get_all_text = file_to_state_fun File.get_all_text

(* [get_highlighted_region st] returns a tuple of the start and end locations
 * of a section of highlighted text *)
let get_highlighted_region st = failwith "Unimplemented"

(* [select_text st l1 l2] selects text from [l1] to [l2] in the open file of [st].
 * Raises Invalid_argument if [l2] comes before [l1]. *)
let select_text st l1 l2 = failwith "Unimplemented"

(* [insert_text st s l] inserts string [s] into the contents the open
 * file of [st] at location [l]. *)
let insert_text st s l = (file_to_state_fun File.insert_text) st s l
  |> replace_current_file st

(* [delete_text st l1 l2] deletes all the text in the currently held
 * file from location [l1] to [l2]. *)
let delete_text st l1 l2 = (file_to_state_fun File.delete_text) st l1 l2
  |> replace_current_file st

(* [undo st] undoes the last change recorded in the open file of [st].
 * If there is nothing left to undo, [undo st] will return [st] unchanged. *)
let undo st = failwith "Unimplemented"

(* [redo st] redoes the last change that was undone in the open file of
 * [st]. If there is nothing left to redo, [redo st] will return [st]
 * unchanged. *)
let redo st = failwith "Unimplemented"

(* [color_text st lst] returns a copy of [st] with the open file now
 * having the color mappings of [lst] *)
let color_text st lst = failwith "Unimplemented"

(* [get_coloring st] gets the coloring scheme of the currently 
 * open file in [st]. *)
let get_coloring st = failwith "Unimplemented"

(* [get_search_term st] gets the current search term in [st]. *)
let get_search_term st = failwith "Unimplemented"

(* [get_search_locations st] returns the list of regions in which
 * the search term has been found in the currently selected file in [st]. *)
let get_search_locations st = failwith "Unimplemented"

(* [find st s] updates [st] so that it holds [s] as its current
 * search term in its currently selected file. *)
let find st s = failwith "Unimplemented"
