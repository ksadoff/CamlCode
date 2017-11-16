(* Abstraction for the type of colors, to be used for coloring text
 * in the terminal *)

open Location

(* Type of a color *)
type color

(* [color_mapping] holds a list of colors and their locations in a file. *)
type color_mapping


(* [make_color rgb] creates a new color from RGB value [rgb]. It looks at
 * the least significant 3 bytes of [rgb], of which the most significant
 * byte is the red value, the second is green, and the last is blue. *)
val make_color : int -> color

(* [cm_to_list cm] takes a color mapping [cm] and returns a list of 
 * tuples [(l1, l2, c)], in which [c] is the color used on the text
 * from locations [l1] to [l2]. *)
val cm_to_list : color_mapping -> (location * location * color) list

