(* The State module contains the entire state of the program,
 * including a list of all files being used. *)

open Color

(* Represents the area where user is typing, i.e. in a file or
 * in the command line. *)
type typing_area

type clipboard

(* State of the program. Contains the following information:
 * * List of files currently open
 * * The typing area that is currently being edited
 * * List of most recently used commands
 * * Clipboard for copy/paste
 * * Name of the current file
 * * First (top) visible line of text
 * * Start and end locations for a block of selected text
 * * Current search term *)
type state

val new_clipboard : clipboard

(* [string_to_clipboard s] converts s into our representation type
 * for clipboard *)
val string_to_clipboard : string -> clipboard

(* [new_file s] creates a new, empty file at path [s].
 * Raises Sys_error creating file failed. *)
val new_file : string -> unit

(* New state with no files open yet *)
val empty_state : state

(* [get_file_names st] returns a list of strings that represent the names of
 * the currently open files. *)
val get_file_names : state -> string list

(* [get_current_file st] returns the file that is currently being manipulated *)
val get_current_file : state -> File.file

(* [set_current_file st f] sets the current file in [st] to [f]. *)
val set_current_file : state -> File.file -> state

(* [get_current_file_name st] returns the string of the name of the file being *)
val get_file_names : state -> string list

 (* [get_current_file st] returns the file that is currently being manipulated *)
val get_current_file : state -> File.file

(* [set_current_file st f] returns a new state with the same fields as st except
 * with the current file set to f*)

(* [get_current_file_name st] returns the string of the name of the file being
 * manipulated. *)
val get_current_file_name : state -> string

(* [open_file st s] constructs the file at path [s] and adds it
 * to the list of files in state [st].
 * Raises Sys_error if file read failed. *)
val open_file : state -> string -> state

(*[is_filed_saved st] returns true if the file is saved and false if not*)
val is_file_saved : state -> bool

(* [save_file st s] saves the currently selected file in [st] at
 * relative path [s].
 * Raises Sys_error if file write failed. *)
val save_file : state -> string -> unit

(* [close_file st] removes the currently selected file [f]
 * from the list of open files in [st]. The newly selected file
 * becomes the file at the beginning of the list of files in [st].
 * If no file is currently selected, returns [st]. *)
val close_file : state -> state

(* [change_selected_file s st] changes the selected file in [st]
 * to the file with name [s].
 * Raises Not_found if [s] is not one of the files open in [st]. *)
val change_selected_file : string -> state -> state


(* [get_clipboard st] returns the current clipboard of st *)
val get_clipboard : state -> clipboard

(* [copy st] returns a copy of state with the text selected in the open file of
 * [st] saved to the clipboard *)
val copy : state -> state

(* [paste st] returns a copy of state with the text from the clipboard of [st]
 * inserted at the cursor location in the open flie of [st] *)
val paste : state -> state

(* [get_cursor_location st] gets the location of the cursor in the file open
 * in [st]. *)
val get_cursor_location : state -> int

(* [get_cursor_line_num st] returns the line number of the cursor in
 * the file that is currently open in [st]. *)
val get_cursor_line_num : state -> int

(* [get_cursor_line_num st] returns the column of the cursor in
 * the file that is currently open in [st]. *)
val get_cursor_column : state -> int

(* [move_cursor st l] moves the cursor of the open file in [st] to [l] *)
val move_cursor : state -> int -> state

(* [cursor_left st] moves the cursor left on the currently selected
 * file in [st]. *)
val cursor_left : state -> state

(* [cursor_right st] moves the cursor right on the currently selected
 * file in [st]. *)
val cursor_right : state -> state

(* [cursor_up st] moves the cursor up on the currently selected file
 * in [st]. *)
val cursor_up : state -> state

(* [cursor_down st] moves the cursor down on the currently selected file
 * in [st]. *)
val cursor_down : state -> state

(* [scroll_to st n] changes the line number of the scrolled view of
 * the file open in [st] to to [n]. *)
val scroll_to : state -> int -> state

(* [get_scroll_line st] returns the first visible line in the
 * currently selected file in [st]. *)
val get_scroll_line : state -> int

(* [get_text st l1 l2] returns all text in the open file of [st] from
 * [l1] to [l2]. Raises Invalid_argument if [l2] comes before [l1].  *)
val get_text : state -> int -> int -> string

(* [get_all_text st] returns a string representing all of the text in
 * the file opened in [st] *)
val get_all_text : state -> string

(* [select_text st l1 l2] selects text from [l1] to [l2] in the currently
 * selected file in [st]. This function forces [l1] and [l2] to be in order
 * and in bounds. *)
val select_text : state -> int -> int -> state

(* Returns [st] with no selected text in its current file. *)
val unselect_text : state -> state

(* [get_selected_range st] returns [None] if no text is selected in the
 * current file in [st], or [Some (i1, i2)] if there is currently text
 * selected from index [i1] to [i2]. *)
val get_selected_range : state -> (int * int) option

(* [insert_text st s l] inserts string [s] into the contents the open
 * file of [st] at location [l]. *)
val insert_text : state -> string -> int -> state

(* [delete_text l1 l2] deletes all the text in the currently held
 * file from location [l1] to [l2]. *)
val delete_text : state -> int -> int -> state

(* [undo st] undoes the last change recorded in the open file of [st].
 * If there is nothing left to undo, [undo st] will return [st] unchanged. *)
val undo : state -> state

(* [redo st] redoes the last change that was undone in the open file of
 * [st]. If there is nothing left to redo, [redo st] will return [st]
 * unchanged. *)
val redo : state -> state

(* [color_text st lst] returns a copy of [st] with the open file now
 * having the color mappings of [lst] *)
val color_text : state -> color_mapping -> state

(* [get_coloring st] gets the coloring scheme of the currently
 * open file in [st]. *)
val get_coloring : state -> color_mapping

(* [get_search_term st] gets the current search term in [st]. *)
val get_search_term : state -> string

(* [get_search_locations st] returns the list of regions in which
 * the search term has been found in the currently selected file in [st]. *)
val get_search_locations : state -> (int *int ) list

(* [find st s] updates [st] so that it holds [s] as its current
 * search term in its currently selected file. *)
val find :  string -> state -> state
