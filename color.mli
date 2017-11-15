(* Abstraction for the type of colors, to be used for coloring text
 * in the terminal *)

(* Type of a color *)
type color

(* [color_mapping] holds a list of colors and their locations in a file. *)
type color_mapping

(* [make_color i] *)
val make_color : int -> color