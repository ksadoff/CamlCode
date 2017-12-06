(* Abstraction for the type of colors, to be used for coloring text
 * in the terminal *)

(* Type of a color: Represented as a triple, the first int represents *)
type color = (int * int * int)

(* [color_mapping] holds a list of colors and their locations in a file. *)
type color_mapping = (int * color) list

(* [fst3 (f, _, _)] takes in a triple and returns the first element of the 
 * triple *)
let fst3 (f, _, _) = f

(* [snd3 (_, s, _)] takes in a triple and returns the second element of the 
 * triple *)
let snd3 (_, s, _) = s

(* [thr3 (_, _, t)] takes in a triple and returns the third element of the 
 * triple *)
let thr3 (_, _, t) = t

let compare' lc1 lc2 = 
  if (fst lc1) < (fst lc2) then (-1)
  else if (fst lc1) = (fst lc2) then 0
  else if (fst lc1) > (fst lc2) then 1
  else failwith "compare_Impossible"

(* 
 * [rgb_norm clr] takes in an integer [clr] and returns [clr] if it is a 
 * valid RGB value (i.e. 0 <= clr <= 255). If it less than 0, then it will 
 * return 0, and if it is greater than 255, then it will return 255.
 *)
let rgb_norm clr = 
  if clr < 0 then 0
  else if clr > 255 then 255
  else clr

(* [valid_color rgb] takes in a [color] and returns the color if it is a valid
 * RGB color, and raises an error otherwise. *)
let valid_color rgb = 
  let r = fst3 rgb |> rgb_norm in 
  let g = snd3 rgb |> rgb_norm in 
  let b = thr3 rgb |> rgb_norm in 
  (r, g, b)

(* 
 * [make_color (r, g, b)] creates a new color from RGB value [(r, g, b)].
 * The first element will represent the red value, the second element will
 * represent the green value, and the third element will represent the 
 * blue value. 
*)
let make_color (r, g, b) = (r, g, b) 

(* 
 * [cm_to_list cm] takes a color mapping [cm] and returns a list of 
 * tuples [(loc, c)], in which [c] is the color used on the text
 * from locations [loc] to either the end of the file, or until the location
 * specified in the next element of the color mapping.
 *)
let cm_to_list cm = cm

(* 
 *[make_cm l] takes a list of [(loc, c)] tuples where [c] is the
 * color from [loc] to either the end of the file, or until the location
 * specified in the next element of the color mapping and produces a new 
 * color mapping. 
 *)
let make_cm l = l

(* [add_color cm (loc, c)] adds a new color [c] from [l1] to [l2]. 
 * Problem: If there is a current color that exists 
 * in the second location, don't add it. *)
let add_color cm (loc1, loc2, c) = 
  let new_list = List.filter (fun x -> (fst x) <> loc1) cm in
  if(List.exists (fun x -> (fst x) = loc2) new_list) then
    List.sort compare ((loc1, c)::new_list)
  else
    List.sort compare ((loc1, c)::(loc2, (255, 255, 255))::new_list) 

(* [empty_cm] is the default color mapping in which all text is the
 * default color. *)
let empty_cm = [(0, (255, 255, 255))]