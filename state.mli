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