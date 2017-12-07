(* The Controller module is responsible for getting input from the user
 * either as keybindings within the file or as a command in the command
 * prompt. It then uses the State module to update the current state of
 * the editor and the CLView module to update the display. *)

open State
open Clview
open Lwt

(* Function that begins running the text editor with no files open. *)
val main : unit -> unit Lwt.t


(* [repl ui stref] reads input from the user, evaluates it to a new state,
 * changes the UI accordingly, and repeats. Returns when user quits. *)
val repl : LTerm_ui.t -> State.state ref -> unit Lwt.t
