(* The State module contains the entire state of the program,
 * including a list of all files being used. *)

open Color

(* Represents the area where user is typing, i.e. in a file or
 * in the command line. *)
type typing_area = unit

(* State of the program. Contains the following information:
 * * List of files currently open
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

  (* currently open file *)
  current_file: File.file option;
}

(* [file_to_state_fun f_fun st] takes a function that acts on a file
 * [f_fun : file -> 'a] and returns a function of type [state -> 'a]
 * that calls to [f_fun] but uses [st.current_file] as input. *)
let file_to_state_fun f_fun st = 
  match st.current_file with 
  | Some f -> f_fun f
  | None -> raise (Invalid_argument "no file selected")

(* [new_file s] creates a new, empty file at path [s].
 * Raises Sys_error creating file failed. *)
let new_file s = let ch_out = open_out s in close_out ch_out

(* New state with no files open yet *)
let empty_state = 
  {
    files = [];
    current_file = None;
  }

(* [open_file st s] constructs the file at path [s] and adds it
 * to the list of files in state [st].
 * Raises Sys_error if file read failed. *)
let open_file st s = 
  let new_file = File.open_file s in 
  {
    files = (s, new_file) :: st.files;
    current_file = Some new_file;
  }

(* [is_filed_saved st] returns true if the file is saved and false if not*)
let is_file_saved st = failwith "Unimplemented"

(* [save_file st] saves the currently selected file in [st] at
 * its corresponding path.
 * Raises Sys_error if file write failed. *)
let save_file = file_to_state_fun File.save_file

(* [close_file st] removes the currently selected file [f]
 * from the list of open files in [st]. The newly selected file
 * becomes the file that occurs before [f] in the list in [st]. *)
let close_file st = failwith "Unimplemented"

(* [change_selected_file s st] changes the selected file in [st]
 * to the file with name [s].
 * Raises Not_found if [s] is not one of the files open in [st]. *)
let change_selected_file s st = failwith "Unimplemented"

(* [copy st] returns a copy of state with the text selected in the open file of
 * [st] saved to the clipboard *)
let copy st = failwith "Unimplemented"

(* [paste st] returns a copy of state with the text from the clipboard of [st]
 * inserted at the cursor location in the open flie of [st] *)
let paste st = failwith "Unimplemented"

(* [get_cursor_location st] gets the location of the cursor in the file open
 * in [st]. *)
let get_cursor_location st = failwith "Unimplemented"

(* [move_cursor st l] moves the cursor of the open file in [st] to [l] *)
let move_cursor st l = failwith "Unimplemented"

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
  |> fun f -> {
    files = 
      let fname = File.get_name f in 
      (fname, f) :: (List.remove_assoc fname);
    current_file = Some f;
  }

(* [delete_text st l1 l2] deletes all the text in the currently held
 * file from location [l1] to [l2]. *)
let delete_text st l1 l2 = failwith "Unimplemented"

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
