(* The State module contains the entire state of the program,
 * including a list of all files being used. *)

open Color

(* Indicates whether or not a file is open *)
type opened_file

type typing_area = Command | File

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

(* [clipboard_to_string st] converts our representation type for clipboard
 * into a string*)
val clipboard_to_string : state -> string

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

(* [is_on_file st] returns [true] if there user is currently on a file,
 * and [false] if the user does not have a file open or if they
 * are typing on the command prompt. *)
val is_on_file : state -> bool

(* [get_current_file_name st] returns the string of the name of the file being
 * manipulated. *)
val get_current_file_name : state -> string

(* [get_typing_area st] returns the typing area of [st], either the command
 * prompt or a file *)
val get_typing_area : state -> typing_area

(* [toggle_typing_area st] returns a copy of [st] with its typing area swapped *)
val toggle_typing_area : state -> state

(* [open_file st s] constructs the file at path [s] and adds it
 * to the list of files in state [st]. Additionally, it sets the current_file to
 * the file at path [s]. (i.e. the file we are trying to open)
 * Raises Sys_error if file read failed. *)
val open_file : state -> string -> state

(* [is_filed_saved st s] returns the file named [s] in state [st] is saved.
 * Raises [Not_found] if file does not exist in [st]. *)
val is_file_saved : state -> string -> bool

(* [save_file st s] saves the currently selected file in [st] at
 * relative path [s].
 * Raises Sys_error if file write failed. *)
val save_file : state -> string -> state

(* [close_file st] removes the currently selected file [f]
 * from the list of open files in [st]. The newly selected file
 * becomes the file at the beginning of the list of files in [st].
 * If no file is currently selected, returns [st]. *)
val close_file : state -> state

(* [tab_right st] takes in a state and returns a state with the current file 
 * being replaced with the file that appears next in the list of open files. 
 * If the current file is the last file in the list, 
 * then it will return the current file. *)
 val tab_right : state -> state

 (* [tab_left st] takes in a state and returns a state with the current file 
 * being replaced with the file that appears previous in the list of open files. 
 * If the current file is the first file in the list, 
 * then it will return the current file. *)
 val tab_left : state -> state

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
 * inserted at the cursor location in the open file of [st] *)
val paste : state -> state

(* [cut st] returns a copy of state with the text selected in the open file of st
 * deleted from the contents and saved to the clipboard*)
val cut : state -> state

(* [open_terminal st] returns a copy of [st] with both [command_out] and
 * [command_in] set to [Some ""] if they are [None] in [st] which indicates
 * that the terminal is open but no text is displayed. If the terminal is open
 * in [st] it returns [st] *)
val open_terminal : state -> state

(* [close_terminal st] returns a copy of [st] with both [command_out] and
 * and [command_in] both set to [None], indicating that the terminal is closed *)
val close_terminal : state -> state

(* [set_command_out st s] returns a copy of [st] with [command_out] set to
 * [Some s], if the terminal is not open in [st] the returned value also has
 * [command_in] set to [Some ""] *)
val set_command_out : state -> string -> state

(* [get_command_out st] returns the [command_out] field of [st] *)
val get_command_out : state -> string option

(* [set_command_in st s] returns a copy of [st] with [command_in] set to
 * [Some s], if the terminal is not open in [st] the returned value also has
 * [command_out] set to [Some ""] *)
val set_command_in : state -> string -> state

(* [get_command_out st] returns the [command_out] field of [st] *)
val get_command_in : state -> string option

(* [cmd_insert st c] returns a copy of [st] with [c] inserted at the command
 * cursor location in the command input and the cursor moved one space right *)
val cmd_insert : state -> char -> state

(* [cmd_delete st c] returns a copy of [st] with the character at the location
 * of the command cursor in the command input deleted and the command cursor moved
 * one space left *)
val cmd_delete : state -> state

(* [get_cmd_cursor st] returns the location of the cursor in the command prompt *)
val get_cmd_cursor : state -> int

(* [cmd_cursor_right st] returns a copy of [st] with the command cursor moved
 * one space to the right *)
val cmd_cursor_right : state -> state

(* [cmd_cursor_left st] returns a copy of [st] with the command cursor moved
 * one space to the left *)
val cmd_cursor_left : state -> state

(* [get_cmd_text st] returns a single character string located at the location
 * of the command cursor in the command input, or " " if the command input is
 * empty. Assumes the command input exists *)
val get_cmd_text : state -> string

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

(* [start_selecting st] sets the fixed selecting point to the current
 * location of the cursor in the currently selected file in [st]. *)
val start_selecting : state -> state

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

(* [get_select_start st] returns [Some (i, l, c)] where [i]
 * is the index of the beginning of the selection region, [l] is the line
 * number, and [c] is the column. If not selection has been made,
 * returns None. *)
val get_select_start : state -> (int * int * int) option

(* [insert_text st s l] inserts string [s] into the contents the open
 * file of [st] at location [l]. *)
val insert_text : state -> string -> int -> state

(* [insert_char st c] inserts a character [c] at the cursor position
 * in the currently selected file in [st] and moves the cursor one
 * position to the right. *)
val insert_char : state -> char -> state

(* [delete_text l1 l2] deletes all the text in the currently held
 * file from location [l1] to [l2]. *)
val delete_text : state -> int -> int -> state

(* [delete_char st] deletes the character before the cursor postion
 * in the currently selected file in [st] and moves the cursor
 * to the left accordingly. *)
val delete_char : state -> state

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
val get_search_term : state -> string option

(* [select_search_term st] returns an updated version of [st] with the currently selected file
 * with the next instance of the search term selected. The next instance is
 * defined as from the currently selected text. If no text is selected, the
 * new version of the selected file will have the first instance of its search term selected.
 * If there is no search term or it is not found, returns [st] with the selected file no text
 * selected *)
val select_search_term : state -> state

(* [find st s] updates [st] so that it holds [Some s] as its current
 * search term in its currently selected file. Unless [s] = "" or "\n",
 * for which it sets the term to [None] *)
val find :  state -> string -> state

(* [remove_search_term st] removes the search_term of file currently selected
 * in [st] *)
val remove_search_term: state -> state

(* [set_replace_term st s] sets the replace term of file opened in [st] to
 *  to [Some s] unless s = "" or "\n" *)
val set_replace_term: state -> string -> state

(* [remove_replace_term st] sets the replace term of file opened in [st] to [None] *)
val remove_replace_term: state -> state

(* [get_replace_term st] returns [Some s] where [r] is the replacement term
 * if the is no replacement term returns [None] *)
val get_replace_term: state -> string option

(* [replace_next st] calls [File.replace_next f] where [f] is the currently
 * selected file in [st] and changes the currectly selected file to be the
 * the returned file *)
val replace_next: state -> state

(* [replace_all st] calls [File.replace_all f] where [f] is the currently
 * selected file in [st] and changes the currectly selected file to be the
 * the returned file *)
val replace_all: state -> state

(* [num_open_files st] returns the number of currently open files*)
val num_open_files : state -> int

(* is_on_file st] returns true if the current file has a name or false if not *)
val is_on_file : state -> bool
