(* A location represents a place in a file where someone's
 * cursor could be located. This will be used for displaying
 * a cursor, selecting text, find/replace, etc. *)
type location

(* [make_location n c] creates a new location at line number [n]
 * and column number [c]. *)
val make_location : int -> int -> location

(* [get_line l] returns the line number of [l]. *)
val get_line : location -> int

(* [get_col l] returns the column number of [l]. *)
val get_col : location -> int
