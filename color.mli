(* Abstraction for the type of colors, to be used for coloring text
 * in the terminal *)

(* Type of a color *)
type color

(* [color_mapping] holds a list of colors and their locations in a file. *)
type color_mapping


(* [make_color rgb] creates a new color from RGB value [rgb]. It looks at
 * the least significant 3 bytes of [rgb], of which the most significant
 * byte is the red value, the second is green, and the last is blue. *)
val make_color : (int * int * int) -> color

(* [cm_to_list cm] takes a color mapping [cm] and returns a list of 
 * tuples [(l1, l2, c)], in which [c] is the color used on the text
 * from locations [l1] to [l2]. *)
val cm_to_list : color_mapping -> (int * color) list

(* [make_cm l] takes a list of [(l1, l2, c)] tuples where [c] is the
 * color from [l1] to [l2] and produces a new color mapping. *)
val make_cm : (int * color) list -> color_mapping

(* [add_color cm (l1, l2, c)] adds a new color [c] from [l1] to [l2]. *)
val add_color : color_mapping -> (int * int * color) -> color_mapping

(* [empty_cm] is the default color mapping in which all text is the
 * default color. *)
val empty_cm : color_mapping
