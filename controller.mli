(* The Keyboard module is responsible for getting input from the user
 * either as keybindings within the file or as a command in the command
 * prompt. It then uses the State module to update the current state of
 * the editor and the Terminal module to update the display. *)

open State

(* Function to be called when starting the text editor
 * [main str] opens a file with the name [str] if it exists, the editor opens
 * but with no file open
 * The argument should be optional, when not given a string the
 * function is the same as if the file was not found *)
val main : string -> unit

(* [repl s] waits for input from the user. Once input is recieved it creates
 * a new state, updates the display according to the new state and passes the
 * new state to another call of [repl] recursively*)
val repl : state -> state
