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

(* [save_file f] saves [f] at its corresponding path.
 * Rasis Sys_error if file write failed. *)
val save_file : file -> unit

(* [get_cursor_location f] gets the location of the cursor in [f]. *)
val get_cursor_location : file -> int

(* [get_cursor_line_num f] gets the line number of the cursor in [f]. *)
val get_cursor_line_num : file -> int

(* [get_line_lengths f] returns the list of the lengths of lines
 * in the contents of [f], in order from top of file to bottom. *)
val get_line_lengths : file -> int list

(* [move_cursor f l] moves the cursor location in [f] to [l]. *)
val move_cursor : file -> int -> file

(* [scroll_to f n] changes the line number of the scrolled view
 * to [n]. *)
val scroll_to : file -> int -> file

(* [get_text f l1 l2] returns all text in [f] from [l1] to [l2].
 * Raises Invalid_argument if [l2] comes before [l1].  *)
val get_text : file -> int -> int -> string

(* [get_all_text f] returns a string representing all of the text in [f] *)
val get_all_text : file -> string

(* [select_text f l1 l2] selects text from [l1] to [l2].
 * Raises Invalid_argument if [l2] comes before [l1]. *)
val select_text : file -> int -> int -> file

(* [insert_text f s l] inserts string [s] into the contents
 * of [f] at location [l]. *)
val insert_text : file -> string -> int -> file

(* [delete_text l1 l2] deletes all text in [f] from location 
 * [l1] to [l2]. *)
val delete_text : file -> int -> int -> file

(* [undo f] undoes the last change recorded in [f]. If there
 * is nothing left to undo, [undo f] will return [f] unchanged. *)
val undo : file -> file

(* [redo f] redoes the last change that was undone in [f]. If there
 * is nothing left to redo, [redo f] will return [f] unchanged. *)
val redo : file -> file

(* [color_text f lst] returns a copy of [f] with the color mappings of [lst] *)
val color_text : file -> (int * int * color) list -> file

(* [get_coloring f] gets the coloring scheme of [f]. *)
val get_coloring : file -> color_mapping

(* [get_search_term f] gets the current search term in [f]. *)
val get_search_term : file -> string

(* [get_search_locations f] returns the list of regions in which
 * the search term has been found in [f]. *)
val get_search_locations : file -> (int * int) list

(* [find f s] updates [f] so that it holds [s] as its current
 * search term. *)
val find :  file -> string -> file