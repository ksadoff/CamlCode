(* 
(* The CLView module represents the View portion of the MVC architecture. This
   module takes care of presenting the state to the user in the terminal
   the lambda-term library. *)
open State

(* [init ()] creates a new UI view. *)
val init : unit -> unit

(*[update] is called by [repl] in keyboard.mli after user input is received. It
  takes in the current state, updates the user interface to reflect any change
  the user made to the state, and returns a unit. *)
val update: state -> unit
*)