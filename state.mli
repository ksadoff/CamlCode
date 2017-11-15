(* still need delete*)


(* The State module contains the entire state of the program,
 * including a list of all files being used. *)

open File
open Location
open Color

(* Represents the area where user is typing, i.e. in a file or
 * in the command line. *)
type typing_area

(* State of the program. Contains the following information:
 * * List of files currently open
 * * The typing area that is currently being edited
 * * List of most recently used commands
 * * Clipboard for copy/paste
 * * Name of the current file
 * * First (top) visible line of text
 * * Start and end locations for a block of selected text
 * * Current search term*)
type state

(* [new_file s] creates a new, empty file at path [s].
 * Raises Sys_error creating file failed. *)
val new_file : string -> unit

(* [open_file_state s st] constructs the file at path [s] and adds it
 * to the list of files in state [st].
 * Raises Sys_error if file read failed. *)
val open_file_state : string -> state -> state

(*[is_filed_saved st] returns true if the file is saved and false if not*)
val is_file_saved : state -> bool

(* [save_file_state st] saves the currently selected file in [st] at
 * its corresponding path.
 * Raises Sys_error if file write failed. *)
val save_file_state : state -> unit

(* [close_file_state st] removes the currently selected file [f]
 * from the list of open files in [st]. The newly selected file
 * becomes the file that occurs before [f] in the list in [st]. *)
val close_file : state -> state

(* [change_selected_file s st] changes the selected file in [st]
 * to the file with name [s].
 * Raises Not_found if [s] is not one of the files open in [st]. *)
val change_selected_file : string -> state -> state

(* [copy st] returns a copy of state with the text selected in the open file of
 * [st] saved to the clipboard *)
val copy : state -> state

(* [paste st] returns a copy of state with the text from the clipboard of [st]
 * inserted at the cursor location in the open flie of [st] *)
val paste : state -> state

(* [get_cursor_location st] gets the location of the cursor in the file open
 * in [st]. *)
val get_cursor_location_state : state -> location

(* [move_cursor_state st l] moves the cursor of the open file in [st] to [l] *)
val move_cursor_state : state -> location -> state

(* [scroll_to_state st n] changes the line number of the scrolled view of
 * the file open in [st] to to [n]. *)
val scroll_to_state : state -> int -> state

(* [get_scroll_line_number st] returns the first visible line in state *)
val get_scroll_line_number : state -> int

(* [get_text_state st l1 l2] returns all text in the open file of [st] from
 * [l1] to [l2]. Raises Invalid_argument if [l2] comes before [l1].  *)
val get_text_state : state -> location -> location -> string

(* [get_all_text_state st] returns a string representing all of the text in
 * the file opened in [st] *)
val get_all_text : state -> string

(* [get_highlighted_region st] returns a tuple of the start and end locations
 * of a section of highlighted text *)
val get_highlighted_region : state -> (location*location)

(* [select_text st l1 l2] selects text from [l1] to [l2] in the open file of [st].
 * Raises Invalid_argument if [l2] comes before [l1]. *)
val select_text_state : state -> location -> location -> state

(* [insert_text_state st s l] inserts string [s] into the contents the open
 * file of [st] at location [l]. *)
val insert_text_state : state -> string -> location -> state

(* [undo_state st] undoes the last change recorded in the open file of [st].
 * If there is nothing left to undo, [undo_state st] will return [st] unchanged. *)
val undo_state : state -> state

(* [redo_state st] redoes the last change that was undone in the open file of
 * [st]. If there is nothing left to redo, [redo_state st] will return [st]
 * unchanged. *)
val redo_state : state -> state

(* [color_text_state st lst] returns a copy of [st] with the open file now
 * having the color mappings of [lst] *)
val color_text_state : state -> color_mapping -> state

val get_color_mapping : state -> color_mapping

val get_search_term : state -> string

val get_search_locations : state -> (location*location) list

(*[find st str] takes in a string and a state and returns an updated state*)
val find :  string -> state -> state
