(* Abstraction for the type of colors, to be used for coloring text
 * in the terminal *)

(* Type of a color *)
type color

(* [make_color i] *)
val make_color : int -> color
