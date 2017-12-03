
(* The CLView module represents the View portion of the MVC architecture. This
   module takes care of presenting the state to the user in the terminal
   the lambda-term library. *)
(* open State *)
open LTerm_ui
open LTerm_draw
(*[update] is called by [repl] in command.ml after user input is received. It
  takes in the current state, updates the user interface to reflect any change
  the user made to the state, and returns a unit. *)
(* val update: state -> unit *)

(* type coord *)
val draw : LTerm_ui.t -> LTerm_draw.matrix -> LTerm_geom.coord -> unit
