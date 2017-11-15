(* Signature class used by all plugin modules *)

open File
open State
open Keystroke
open Color

(* List of string commands that can be entered into the editor terminal.
 * Each command is written as [(s, f)] where [s] is the keyword that
 * must be written at the beginning of the command, and [f] is a function
 * that takes current state [st] of the editor and the argument [a]
 * of the command, written as a string. [f] returns a new state. *)
val commands : (string * (state -> string -> state)) list

(* List of keyboard commands. Each command is written as [(kb, f)], where
 * [kb] is the keyboard command, and [f] is a function that modifies
 * the editor state. *)
val kb_commands : (keystroke * (state -> state)) list

(* [text_coloring s] creates a new coloring for a file. *)
val text_coloring : file -> color_mapping