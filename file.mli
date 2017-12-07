(* The File module contains types and functions used for manipulating
 * a single file. It is technically part of the "model" in the MVC
 * architecture, but it only includes file-specific things. *)

open Color

(* A contents variable represents the entire contents of a file,
 * including all characters. *)
type contents

(* A file variable represents all the state that is recorded
 * for one file. It should contain the following information:
 * * file name/relative path
 * * file contents
 * * location of cursor
 * * line number of where current view begins
 * * beginning and end locations of highlighted text
 * * text copied to clipboard
 * * whether file has been saved since last change
 * * last k changes (used for undo/redo)
 * * current search term (used in find/replace) *)
type file

(* [open_file s] reads the contents of the file stored at
 * relative path [s] and uses that to construct a new file type.
 * Raises Sys_error if opening file failed. *)
val open_file : string -> file

(* [save_file f s] saves [f] at relative path [s].
 * Raises Sys_error if file write failed. *)
val save_file : file -> string -> file

(* [is_saved f] returns whether [f] was saved since the last modification. *)
val is_saved : file -> bool

(*
(* [get_file_contents f] returns the rope that represents the context of the
file *)
val get_file_contents : file -> Rope.t *)

(* [get_cont_length f] returns the length of the file_contents of [f]. *)
val cont_length : file -> int

(* [get_name f] is the relative path of [f]. *)
val get_name : file -> string

(* [get_cursor_location f] gets the location of the cursor in [f]. *)
val get_cursor_location : file -> int

(* [get_cursor_line_num f] gets the line number of the cursor in [f]. *)
val get_cursor_line_num : file -> int

(* [get_cursor_column f] gets the column number of the cursor in [f]. *)
val get_cursor_column : file -> int

(* [get_line_lengths f] returns the list of the lengths of lines
 * in the contents of [f], in order from top of file to bottom. *)
val get_line_lengths : file -> int list

(* [move_cursor f l] moves the cursor location in [f] to [l]. *)
val move_cursor : file -> int -> file

(* [cursor_left f] returns [f] with cursor moved one position left.
 * If the end of the line is reached, cursor moves to end of previous
 * line. If cursor at index 0, it doesn't move. *)
val cursor_left : file -> file

(* [cursor_right f] returns [f] with cursor moved one position right.
 * If the end of the line is reached, cursor moves to beginning
 * of next line. If cursor at the end of file, it doesn't move. *)
val cursor_right : file -> file

(* [cursor_up f] returns [f] with cursor moved one line up.
 * If the cursor is farther right then the length of the line it
 * moved to, then the cursor goes at the end of the line.
 * If on first line, cursor goes to farthest left position. *)
val cursor_up : file -> file

(* [cursor_up_scroll f] calls cursor_up f and updates the top visible line based
 * on where the cursor is.*)
val cursor_up_scroll : file -> int -> int -> file

(* [cursor_down_scroll f] calls cursor_up f and updates the top visible line based
 * on where the cursor is.*)
val cursor_down_scroll : file -> int -> int -> file

(* [cursor_right_scroll f] calls cursor_up f and updates the top visible line based
 * on where the cursor is.*)
val cursor_right_scroll : file -> int -> int -> file

(* [cursor_lft_scroll f] calls insert_char f and updates the top visible line based
 * on where the cursor is.*)
val cursor_left_scroll : file -> int -> int -> file

(* [insert_scroll f] calls delete_char f and updates the top visible line based
 * on where the cursor is.*)
val insert_scroll : file -> char -> int -> int -> file

(* [delete_scroll f c] calls delete_char f c and updates the top visible line based
 * on where the cursor is.*)
val delete_scroll : file -> int -> int -> file

(* [cursor_down f] returns [f] with cursor moved one line down.
 * If the cursor is farther right then the length of the line it
 * moved to, then the cursor goes at the end of the line.
 * If on last line, cursor goes to farthest right position. *)
val cursor_down : file -> file

(* [scroll file w h] returns a copy of [f] with the top visible line
 * set relative to where the cursor is*)
val scroll : file -> int -> int -> file

(* [scroll_to f n] changes the line number of the scrolled view
 * to [n]. *)
val scroll_to : file -> int -> file

(* [get_scroll_line f] returns the highest line that view is currently
 * scrolled to *)
val get_scroll_line : file -> int

(* [get_text f l1 l2] returns all text in [f] from [l1] to [l2].
 * Raises Invalid_argument if [l2] comes before [l1].  *)
val get_text : file -> int -> int -> string

(* [get_line_text f ln] is the text in [f] at line number [ln]. *)
val get_line_text : file -> int -> string

(* [get_scrolled_lines st w h] displays the currently scrolled to lines,
 * so that the cursor is viewable horizontally and the first line displayed
 * is the current scroll line. [w] is the max width of each line,
 * and [h] is the max number of lines. *)
val get_scrolled_lines : file -> int -> int -> string

(* [get_all_text f] returns a string representing all of the text in [f] *)
val get_all_text : file -> string

(* [start_selecting f] sets the fixed selecting point to the current
 * location of the cursor in [f]. *)
val start_selecting : file -> file

(* [select_text f l1 l2] selects text from [l1] to [l2].
 * This function forces [l1] and [l2] to be in order and in bounds.
 * The selection point is set to [l1] and the cursor is set to [l2]. *)
val select_text : file -> int -> int -> file

(* Returns [f] with no selected text. *)
val unselect_text : file -> file

(* [get_selected_range f] returns the [None] if no text is selected,
 * or [Some (i1, i2)] if there is currently text selected from index
 * [i1] to [i2]. *)
val get_selected_range : file -> (int * int) option

(* [get_select_start f] returns [Some (i, l, c)] where [i]
 * is the index of the beginning of the selection region, [l] is the line
 * number, and [c] is the column. If not selection has been made,
 * returns None. *)
val get_select_start : file -> (int * int * int) option

(* [get_select_point f] returns [Some (i, l, c)] where [i]
 * is the index of the fixed selection point, [l] is the line number,
 * and [c] is the column. If no selection has been made, returns [None]. *)
val get_select_point : file -> (int * int * int) option

(* [insert_text f s] inserts string [s] into the contents
 * of [f] at location [l]. The beginning of the inserted string
 * will be at index [l]. If [l] is an invalid location, the closest
 * valid location will be used. *)
val insert_text : file -> string -> int -> file

(* [insert_char f c] inserts a character [c] into the contents of [f]
 * at the cursor location in [f]. *)
val insert_char : file -> char -> file

(* [delete_text l1 l2] deletes all text in [f] from location
 * [l1] to [l2]. *)
val delete_text : file -> int -> int -> file

(* [delete_char f] deletes the character directly to the left of the
 * cursor in [f] and moves the cursor left one character. If there
 * is no character before the cursor, the file is left unchanged. *)
val delete_char : file -> file

(* [undo f] undoes the last change recorded in [f]. If there
 * is nothing left to undo, [undo f] will return [f] unchanged. *)
val undo : file -> file

(* [redo f] redoes the last change that was undone in [f]. If there
 * is nothing left to redo, [redo f] will return [f] unchanged. *)
val redo : file -> file

(* [color_text f lst] returns a copy of [f] with the color mappings of [lst] *)
val color_text : file -> color_mapping -> file

(* [get_coloring f] gets the coloring scheme of [f]. *)
val get_coloring : file -> color_mapping

(* [get_search_term f] gets the current search term in [f]. *)
val get_search_term : file -> string option

(* [select_search_term f] returns an updated version of [f] with
 * with the next instance of the search term selected. The next instance is
 * defined as from the currently selected text. If no text is selected the
 * new version of [f] will have the first instance of its search term selected.
 * If there is no search term or it is not found, returns [f] with no text
 * selected *)
val select_search_term : file -> file

(* [find f s] updates [f] so that it holds [s] as its current
 * search term.  Unless [s] = "" or "\n",
 * for which it sets the term to [None] *)
val find :  file -> string -> file

(* [remove_search_term f] removes the search_term of file [f] *)
val remove_search_term: file -> file

(* [set_replace_term f s] sets the replace term of file [f] to [Some s]
 * unless s = "" or "\n", for which it sets the term to [None] *)
val set_replace_term: file -> string -> file

(* [remove_replace_term f] sets the replace term of file [f] to [None]*)
val remove_replace_term: file -> file

(* [get_replace_term f] returns [Some s] where [r] is the replacement term
 * if the is no replacement term returns [None] *)
val get_replace_term: file -> string option

(* [replace_next f] returns an updated copy of [f] where the next instance
 * of the search term is replaced by the replace term, which is now selected
 * in the file. The next instance is
 * defined as from the currently selected text. If no text is selected the
 * new version of [f] will replace the first instance of its search term.
 * If there is no instance of the search term or either the search or replace
 * term does not exist, returns [f] with no text selected *)
val replace_next: file -> file

(* [replace_all f] returns an updated copy of [f] where the every instance
 * of the search term is replaced by the replace term.
 * If there is no instance of the search term or either the search or replace
 * term does not exist, returns [f] with no text selected *)
val replace_all: file -> file

(* [get_visible_text f numlines] returns the text from the file's scroll_line_num
 * to the line num_lines below it *)
val get_visible_text : file -> int -> string

(* [first_index_of_line f linenum] returns the index in the file contents that
 * corresponds to the first index of the line at linenum in the list of
 * line lengths *)
val first_index_of_line : file -> int -> int

(* [last_index_of_line f linenum] returns the index in the file contents that
 * corresponds to the last index of the line at linenum in the list of
 * line lengths*)
val last_index_of_line : file -> int -> int
