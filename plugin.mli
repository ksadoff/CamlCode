(* Signature used by all plugin modules *)

open File
open State
open Color

(* Type of commands that can be input by the user. *)
type command

(* [parse_command s] converts a raw input command [s] from the user
 * to a command. Returns [None] if [s] is not a command for this plugin. *)
val parse_command : string -> command option

(* [execute_command cmd st] changes the state based on command [cmd]. *)
val execute_command : command -> state -> state

(* [respond_to_event event st] changes the state based on some event,
 * such as a keyboard shorctut. *)
val respond_to_event : LTerm_event.t -> state -> state

(* [text_coloring f] creates a new coloring for file [f].
 * NOTE: This function is unimplemented and something we would have worked on,
 * given more time.*)
val text_coloring : file -> color_mapping
