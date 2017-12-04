(* The CLView module represents the View portion of the MVC architecture. This
 * module takes care of presenting the state to the user in the terminal
 * the lambda-term library. *)

open State
open LTerm_ui
open LTerm_draw

(* [draw term stref] draws the current state stored at [stref] on the
 * terminal [term]. *)
val draw : LTerm.t  -> state ref -> LTerm_ui.t Lwt.t
