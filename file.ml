(* The File module contains types and functions used for manipulating
 * a single file. It is technically part of the "model" in the MVC
 * architecture, but it only includes file-specific things. *)

(* A contents variable represents the entire contents of a file,
 * including all characters. *)
type contents = Rope.t

(* A file variable represents all the state that is recorded
 * for one file. It should contain the following information:
 * * file name/relative path
 * * file contents
 * * location of cursor
 * * line number of where current view begins
 * * beginning and end locations of highlighted text
 * * text copied to clipboard
 * * whether file has been saved since last change
 * * last k changes (used for undo/redo)
 * * current search term (used in find/replace) *)
type file = {
  (* relative file path *)
  name : string; 

  (* file contents *)
  contents : contents; 

  (* index of cursor in contents *)
  cursor : int; 

  (* line number of cursor *)
  cursor_line_num : int;

  (* [get f.line_lengths i] is the number of characters in line [i]
   * of [f.contents] *)
  line_lengths : int array; 
  
  (* view_line_num : int;
  selected_range : location * location;
  clipboard : string;
  was_saved : bool;
  search_term : string; *)
}

(* [find_newlines cont i0] returns a list [l] (not an array)
 * such that the ith element of [l] is the length of the ith line
 * in [cont], starting at contents location [i0]. *)
let rec find_newlines cont i0 = 
  try 
    let linepos = Rope.search_forward_string "\n" cont i0 in
    linepos :: (find_newlines cont (linepos+1))
  with Not_found -> []

(* [get_line_lengths nls nl0] takes a list of of newline characters 
 * [nls] and returns a list of lengths of lines. [nl0] is the number
 * of characters up to the beginning of the first line, so it is
 * 0 for the first call. The length of the line includes the newline 
 * character at the end. *)
let rec get_line_lengths nls nl0 = 
  match nls with 
  | [] -> []
  | h :: t -> (h - nl0 + 1) :: (get_line_lengths t (h + 1))

(* [open_file s] reads the contents of the file sored at
 * relative path [s] and uses that to construct a new file type.
 * Raises Sys_error if opening file failed. *)
let open_file s = 
  let rec append_lines channel rope_acc = 
    try begin 
      let line = input_line channel in 
      let rope_line = Rope.concat2 
        (Rope.of_string line) (Rope.of_string "\n") in 
      Rope.concat2 rope_acc rope_line
        |> append_lines channel
    end 
    with End_of_file -> rope_acc in
  let channel = open_in s in 
  let contents = append_lines channel Rope.empty in 
  {
    name = s;
    contents = contents;
    cursor = 0;
    cursor_line_num = 0;
    line_lengths = find_newlines contents 0 
      |> fun nls -> get_line_lengths nls 0
      |> Array.of_list;
  }

(* [save_file f] saves [f] at its corresponding path.
 * Rasis Sys_error if file write failed. *)
let save_file f = failwith "Unimplemented" 

(* [get_cursor_location f] gets the location of the cursor in [f]. *)
let get_cursor_location f = f.cursor

(* [get_cursor_line_num f] gets the line number of the cursor in [f]. *)
let get_cursor_line_num f = f.cursor_line_num

(* [get_line_lengths f] returns the list of the lengths of lines
 * in the contents of [f], in order from top of file to bottom. *)
let get_line_lengths f = f.line_lengths |> Array.to_list

(* [move_cursor f l] moves the cursor location in [f] to [l].
 * If [l] is an invalid location, the cursor becomes the closest
 * value to [l]. *)
let move_cursor f l = { f 
  with cursor = 
    if l < 0 then 0
    else if l >= Rope.length f.contents then Rope.length f.contents - 1
    else l
    (* TODO: calculate cursor_line_num *)
}

(* [scroll_to f n] changes the line number of the scrolled view
 * to [n]. *)
let scroll_to f n = failwith "Unimplemented" 

(* [get_text f l1 l2] returns all text in [f] from [l1] to [l2].
 * Raises Invalid_argument if [l2] comes before [l1].  *)
let get_text f l1 l2 = failwith "Unimplemented" 

(* [get_all_text f] returns a string representing all of the text in [f] *)
let get_all_text f = Rope.to_string f.contents

(* [select_text f l1 l2] selects text from [l1] to [l2].
 * Raises Invalid_argument if [l2] comes before [l1]. *)
let select_text f l1 l2 = failwith "Unimplemented" 

(* [insert_text f s l] inserts string [s] into the contents
 * of [f] at location [l]. *)
let insert_text f s l = failwith "Unimplemented" 

(* [delete_text l1 l2] deletes all text in [f] from location 
 * [l1] to [l2]. *)
let delete_text l1 l2 = failwith "Unimplemented" 

(* [undo f] undoes the last change recorded in [f]. If there
 * is nothing left to undo, [undo f] will return [f] unchanged. *)
let undo f = failwith "Unimplemented" 

(* [redo f] redoes the last change that was undone in [f]. If there
 * is nothing left to redo, [redo f] will return [f] unchanged. *)
let redo f = failwith "Unimplemented" 

(* [color_text f lst] returns a copy of [f] with the color mappings of [lst] *)
let color_text f lst = failwith "Unimplemented" 

(* [get_coloring f] gets the coloring scheme of [f]. *)
let get_coloring f = failwith "Unimplemented" 

(* [get_search_term f] gets the current search term in [f]. *)
let get_search_term f = failwith "Unimplemented" 

(* [get_search_locations f] returns the list of regions in which
 * the search term has been found in [f]. *)
let get_search_locations f = failwith "Unimplemented" 

(* [find f s] updates [f] so that it holds [s] as its current
 * search term. *)
let find f s = failwith "Unimplemented" 