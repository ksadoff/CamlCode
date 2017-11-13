(* A location represents a place in a file where someone's
 * cursor could be located. This will be used for displaying
 * a cursor, selecting text, find/replace, etc. *)
type location

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
 * * current search term (used in find/replace)
 * * list of locations where search term was found *)
type file 

(* [init_file s] creates a new, empty file at path [s].
 * Raises Sys_error creating file failed. *)
val init_file : string -> unit

(* [open_file s st] reads the contents of the file stored at 
 * relative path [s] and uses that to construct a new file.
 * Raises Sys_error if opening file failed. *)
val open_file : string -> file 

(* [save_file f] saves [f] at its corresponding path.
 * Rasis Sys_error if file write failed. *)
val save_file : file -> unit 

(* [get_line l] returns the line number of [l]. *)
val get_line : location -> int

(* [get_col l] returns the column number of [l]. *)
val get_col : location -> int

(* [get_cursor_location f] gets the location of the cursor in [f]. *)
val get_cursor_location : file -> location

(* [move_cursor f l] moves the cursor location in [f] to [l]. *)
val move_cursor : file -> location -> file 

(* [scroll_to f n] changes the line number of the scrolled view
 * to [n]. *)
val scroll_to : file -> int -> file 

(* [select_text f l1 l2] selects text from [l1] to [l2].
 * Raises Invalid_argument if [l2] comes before [l1]. *)
val select_text : file -> location -> location -> file 

(* [insert_text f s l] inserts string [s] into the contents
 * of [f] at location [l]. *)
val insert_text : file -> string -> location -> file 

(* [undo f] undoes the last change recorded in [f]. If there
 * is nothing left to undo, [undo f] will return [f] unchanged. *)
val undo : file -> file

(* [redo f] redoes the last change that was undone in [f]. If there
 * is nothing left to redo, [redo f] will return [f] unchanged. *)
val redo : file -> file