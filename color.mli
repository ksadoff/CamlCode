(* Abstraction for the type of colors, to be used for coloring text
 * in the terminal *)

(* Type of a color *)
type color

(* [color_mapping] holds a list of colors and their locations in a file. *)
type color_mapping

(* [make_color rgb] creates a new color from RGB value [rgb]. It looks at
 * the least significant 3 bytes of [rgb], of which the most significant
 * byte is the red value, the second is green, and the last is blue. *)
val make_color : int -> color


