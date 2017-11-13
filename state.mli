(* The State module contains the entire state of the program,
 * including a list of all files being used. *)

open File

(* Represents the area where user is typing, i.e. in a file or
 * in the command line. *)
type typing_area

(* State of the program. Contains the following information:
 * * List of files currently open
 * * The typing area that is currently being edited
 * * List of most recently used commands 
 * * Clipboard for copy/paste *)
type state

(* [new_file s] creates a new, empty file at path [s].
 * Raises Sys_error creating file failed. *)
val new_file : string -> unit

(* [open_file_state s st] constructs the file at path [s] and adds it
 * to the list of files in state [st].
 * Raises Sys_error if file read failed. *)
val open_file_state : string -> state -> state

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