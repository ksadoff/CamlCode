(* Signature to abstract the type of a keystroke *)

(* Describes a button on the keyboard. *)
type keybutton

(* A type to describe a keystroke, used for plugins to map keystrokes to
 * functions *)
type keystroke

(* [make_keystroke keys] creates a new keystroke representing the event
 * where a user presses all buttons in [keys]. *)
val make_keystroke : keybutton list -> keystroke