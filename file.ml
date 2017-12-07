(* The File module contains types and functions used for manipulating
 * a single file. It is technically part of the "model" in the MVC
 * architecture, but it only includes file-specific things. *)

(* A contents variable represents the entire contents of a file,
 * including all characters. *)
type contents = Rope.t

(* represents where a cursor can be in text *)
type location = {
  index : int; (* index in string of entire file *)
  line_num : int; (* line number *)
  column : int; (* column number *)
}

type undo_state = {
  text : contents;
  cursor_loc : location;
}

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

  (* cursor location *)
  cursor : location;

  (* [Array.get f.line_lengths i] is the number of characters in
   * line [i] of [f.contents] *)
  line_lengths : int array;

  (* top line that view is currently scrolled to *)
  scroll_line_num : int;

  (* Fixed point where text selection begins/ends.
   * Text selection extends from [selectpoint] to [cursor].
   * The selected range is INCLUSIVE on both endpoints. *)
  selectpoint : location option;

  (* the string that the user is currently searching for *)
  search_term : string option;

  (* the string that will replace the search term *)
  replace_term : string option;

  (* a list of tuples that represent the beginning location of a color, and
   * the color that characters starting at that location should be. *)
  color_mapping : Color.color_mapping;

  (* copied string *)
  clipboard : Rope.t;

  (* whether file was saved since last edit *)
  was_saved : bool;

  (* the number of previous undo_states in the undo stack *)
  num_undos : int;

  (* a stack of previous file states than are used to revert to previous states *)
  undo_list : file list;

  (* number of redos so far *)
  num_redos : int;

  redo_list : file list
}

(* sets the maximum size of the undo queue *)
let max_undos = 50

(* [rem_tail lst] returns a copy of [lst] with the last element removed *)
let rec rem_tail = function
  | []
  | _::[] -> []
  | h::t -> h::(rem_tail t)

(* [add_undo f] returns a copy of f with a new element in the undo stack that
 * contains the text and cursor location of [f], and the size of the stack
 * updated. If the stack is full the bottom element is removed *)
let add_undo f =
  if f.num_undos < max_undos then
    {f with num_undos = f.num_undos + 1;
            undo_list = f::f.undo_list;}
  else
    {f with undo_list = f::(rem_tail f.undo_list);}

(* [add_redo f] returns a copy of f with a new element in the redo stack that
 * contains the text and cursor location of [f], and the size of the stack
 * updated. If the stack is full the bottom element is removed *)
let add_redo f =
  if f.num_redos < max_undos then
    {f with num_redos = f.num_redos + 1;
            redo_list = f::f.redo_list;}
  else
    {f with redo_list = f::(rem_tail f.redo_list);}


(* [get_cont_length f] returns the length of the contents of [f]. *)
let cont_length f = Rope.length f.contents

let get_contents f = f.contents

let set_contents f r = {f with contents = r}

(* [find_newlines cont i0] returns a list [l] (not an array)
 * such that the ith element of [l] is the length of the ith line
 * in [cont], starting at contents location [i0]. *)
let rec find_newlines cont i0 =
  try
    let linepos = Rope.search_forward_string "\n" cont i0 in
    linepos :: (find_newlines cont (linepos+1))
  with Not_found ->
    if Rope.length cont = i0 then [] else [Rope.length cont - 1]

(* [get_line_lengths nls nl0] takes a list of of newline characters
 * [nls] and returns a list of lengths of lines. [nl0] is the number
 * of characters up to the beginning of the first line, so it is
 * 0 for the first call. The length of the line includes the newline
 * character at the end. *)
let rec get_line_lengths nls nl0 =
  match nls with
  | [] -> []
  | h :: t -> (h - nl0 + 1) :: (get_line_lengths t (h + 1))

(* [line_lengths_arr cont] returns an array [a] where
 * [Array.get a i] is the length of the ith line in [cont]. *)
let line_lengths_arr cont =
  find_newlines cont 0
  |> fun nls -> get_line_lengths nls 0
  |> Array.of_list

(* [open_file s] reads the contents of the file stored at
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
  let contents = append_lines channel Rope.empty
   |> fun r -> if Rope.to_string r = "" then Rope.of_string "\n" else r in
  {
    name = s;
    contents = contents;
    cursor = {index=0; line_num=0; column=0};
    line_lengths = line_lengths_arr contents;
    scroll_line_num = 0;
    selectpoint = None;
    search_term = None;
    replace_term = None;
    color_mapping = Color.empty_cm;
    clipboard = Rope.empty;
    was_saved = true;
    num_undos = 0;
    undo_list = [];
    num_redos = 0;
    redo_list = [];
  }

(* [save_file f] saves [f] at relative path [s].
 * Raises Sys_error if file write failed. *)
let save_file f s =
  let ch_out = open_out s in
  Printf.fprintf ch_out "%s" (Rope.to_string f.contents);
  close_out ch_out;
  {f with was_saved = true}

(* [is_saved f] returns whether [f] was saved since the last modification. *)
let is_saved f = f.was_saved

(* [get_name f] is the relative path of [f]. *)
let get_name f = f.name

(* [get_cursor_location f] gets the location of the cursor in [f]. *)
let get_cursor_location f = f.cursor.index

(* [get_cursor_line_num f] gets the line number of the cursor in [f]. *)
let get_cursor_line_num f = f.cursor.line_num

(* [get_cursor_column f] gets the column number of the cursor in [f]. *)
let get_cursor_column f = f.cursor.column

(* [get_line_lengths f] returns the list of the lengths of lines
 * in the contents of [f], in order from top of file to bottom. *)
let get_line_lengths f = f.line_lengths |> Array.to_list

(* requires:
 * [lla] array of line lengths in a file
 * [loc1] previous location of cursor
 * [i2] index of new cursor location
 * returns: location of new cursor location, including line num and column
 * raises: Invalid_argument if any of the following happens
 * * [i1] or [i2] are out of bounds of contents
 * * [ln1] is not a valid index of [lla]
 * * [c1] is not a valid column in its corresponding line
 *)
let rec get_location lla loc1 i2 =
  let {index=i1; line_num=ln1; column=c1} = loc1 in
  (* line number exceptions *)
  if ln1 < 0 || ln1 >= Array.length lla
  then raise (Invalid_argument ("invalid line number " ^ (string_of_int ln1)))
  else
  (* get length of current line and index where it starts *)
  let line_len = Array.get lla ln1 in
  let line_start = i1 - c1 in
  (* column number exceptions *)
  if c1 < 0 || c1 >= line_len
  then raise (Invalid_argument ("invalid column " ^ (string_of_int c1)))
  else
  (* if i1 and i2 on same line, return ln1 *)
  if i2 >= line_start && i2 < line_start + line_len
  then {index=i2; line_num=ln1; column=i2-line_start}
  (* if i2 not on i1's line, recursively call with previous or next line *)
  else
  let new_ln = if i2 < line_start then ln1 - 1 else ln1 + 1 in
  let new_len = Array.get lla new_ln in
  let new_start =
    if i2 < line_start then i1 - c1 - new_len
    else i1 - c1 + line_len in
  get_location lla {index=new_start; line_num=new_ln; column=0} i2

(* [get_line_index lla ln] returns the index of the start of line [ln],
 * given some valid location in [loc] and the array of line lengths [lla].
 * Raises "Invalid_argument" if [ln] is out of range. *)
let rec get_line_index lla loc ln =
  if ln < 0 || ln >= Array.length lla
  then raise (Invalid_argument "invalid line number in get_line_index")
  else
  let line_start = loc.index - loc.column in
  (* base case *)
  if ln = loc.line_num then line_start
  (* line num is before or after cursor *)
  else
  let new_ln = if ln < loc.line_num then loc.line_num - 1
    else loc.line_num + 1 in
  let new_len = Array.get lla new_ln in
  let new_start =
    let {index=i1; line_num=ln1; column=c1} = loc in
    let line_len = Array.get lla ln1 in
    if ln < loc.line_num then i1 - c1 - new_len
    else i1 - c1 + line_len in
  get_line_index lla {index=new_start; line_num=new_ln; column=0} ln

(* [scroll_to f n] changes the line number of the scrolled view
 * to [n]. If [n] is less than 0 or greater than the number of lines in
 * contents, then the closest line number is chosen. *)
let scroll_to f n =
  {f with scroll_line_num =
            let num_lines = Array.length f.line_lengths in
            if n < 0 then 0
            else if n >= num_lines then num_lines - 1
            else n
  }

(* [get_scroll_line f] returns the highest line that view is currently
 * scrolled to *)
let get_scroll_line f = f.scroll_line_num

(* [move_cursor f l] moves the cursor location in [f] to [l]. The cursor
 * index, line number, and column number are all updated. If [l] is an
 * invalid location, the cursor becomes the closest value to [l]. *)
let move_cursor f l =
  let lla = f.line_lengths in
  let l' = if l < 0 then 0
    else if l >= cont_length f then cont_length f - 1
    else l in
  let new_cursor = get_location lla f.cursor l' in
  { f with
    cursor = new_cursor;
  }

(* [cursor_left f] returns [f] with cursor moved one position left.
 * If the end of the line is reached, cursor moves to end of previous
 * line. If cursor at index 0, it doesn't move. *)
let cursor_left f =
  if f.cursor.index = 0 then f
  else if f.cursor.column = 0 then { f with
    cursor = {
      index= f.cursor.index - 1;
      line_num = f.cursor.line_num - 1;
      column =  Array.get f.line_lengths (f.cursor.line_num - 1) - 1;
    };
    (* scroll_line_num = (get_scroll_line f) - 1 *)

  }
  else { f with
    cursor = {
      index = f.cursor.index - 1;
      line_num = f.cursor.line_num;
      column = f.cursor.column - 1;
    }
  }

 (* [cursor_right f] returns [f] with cursor moved one position right.
  * If the end of the line is reached, cursor moves to beginning
  * of next line. If cursor at the end of file, it doesn't move. *)
let cursor_right f =
  let line_len = Array.get f.line_lengths f.cursor.line_num in
  if f.cursor.index = cont_length f - 1 then f
  else if f.cursor.column = line_len - 1 then { f with
    cursor = {
      index = f.cursor.index + 1;
      line_num = f.cursor.line_num + 1;
      column = 0;
    };
    (* scroll_line_num = (get_scroll_line f) + 1 *)
  }
  else { f with
    cursor = {
      index = f.cursor.index + 1;
      line_num = f.cursor.line_num;
      column = f.cursor.column + 1;
    }
  }

 (* [cursor_up f] returns [f] with cursor moved one line up.
  * If the cursor is farther right then the length of the line it
  * moved to, then the cursor goes at the end of the line.
  * If on first line, cursor goes to farthest left position. *)
let cursor_up f =
  let lnum =
    if f.cursor.line_num = 0 then 0
    else f.cursor.line_num - 1 in
  let line_len = Array.get f.line_lengths lnum in
  let col =
    if f.cursor.line_num = 0 then 0
    else if f.cursor.column < line_len then f.cursor.column
    else line_len - 1 in
  let new_cursor =
    if f.cursor.line_num = 0 then 0
    else f.cursor.index - f.cursor.column - line_len + col in
  { f with
    cursor = {
      index = new_cursor;
      line_num = lnum;
      column = col;
    };
    (* scroll_line_num = (get_scroll_line f) - 1 *)
  }


 (* [cursor_down f] returns [f] with cursor moved one line down.
  * If the cursor is farther right then the length of the line it
  * moved to, then the cursor goes at the end of the line.
  * If on last line, cursor goes to farthest right position. *)
let cursor_down f =
  let num_lines = Array.length f.line_lengths in
  let lnum =
    if f.cursor.line_num = num_lines - 1 then num_lines - 1
    else f.cursor.line_num + 1 in
  let prev_line_len = Array.get f.line_lengths f.cursor.line_num in
  let line_len = Array.get f.line_lengths lnum in
  let col =
    if f.cursor.line_num = num_lines - 1 then line_len - 1
    else if f.cursor.column < line_len then f.cursor.column
    else line_len - 1 in
  let new_cursor =
    if f.cursor.line_num = num_lines - 1 then cont_length f - 1
    else f.cursor.index - f.cursor.column + prev_line_len + col in
  { f with
    cursor = {
      index = new_cursor;
      line_num = lnum;
      column = col;
    };
    (* scroll_line_num = (get_scroll_line f) + 1 *)
  }


let scroll f w h =
  if get_cursor_line_num f < get_scroll_line f then
    scroll_to f (get_cursor_line_num f) else
  if get_cursor_line_num f > (get_scroll_line f) + h then
    scroll_to f ((get_cursor_line_num f)-h)
  else f


let cursor_up_scroll f w h =
  cursor_up f |> fun f -> scroll f w h


let cursor_down_scroll f w h =
  cursor_down f |> fun f -> scroll f w h

let cursor_right_scroll f w h =
  cursor_right f |> fun f -> scroll f w h

let cursor_left_scroll f w h =
  cursor_left f |> fun f -> scroll f w h

(* [make_range_valid (i1, i2) cont_len] returns a new pair (i1', i2')
 * such that:
 * if [i1 < 0] or [i2 < 0], then [i1'] or [i2'] is 0
 * if [i1 > cont_len] or [i2 > cont_len], then [i1'] or [i2'] is [cont_len]
 * if [i1 > i2], then i1' is i2 and i2' is i1 *)
let make_range_valid (i1, i2) cont_len =
  let i1' =
    if i1 < 0 then 0
    else if i1 > cont_len then cont_len
    else if i1 > i2 then i2
    else i1 in
  let i2' =
    if i2 < 0 then 0
    else if i2 > cont_len then cont_len
    else if i1 > i2 then i1
    else i2 in
  (i1', i2')

(* [get_text f l1 l2] returns all text in [f] from [l1] to [l2].
 * This function forces [l1] and [l2] to be in order and in bounds. *)
let get_text f l1 l2 =
  let (l1', l2') = make_range_valid (l1, l2) (cont_length f) in
  Rope.sub f.contents l1' (l2' - l1') |> Rope.to_string

(* [get_line_text f ln] is the text in [f] at line number [ln]. *)
let get_line_text f ln =
  let l1 = get_line_index f.line_lengths f.cursor ln in
  let llen = Array.get f.line_lengths ln in
  let l2 = l1 + llen in
  get_text f l1 l2

(* Same as [get_line_text], but scrolls so that the line is at most
 * length [wid]. If the cursor is on line [ln], the line is scrolled
 * horizontally so that the cursor is visible. *)
let get_scrolled_line_text f ln wid =
  if f.cursor.line_num <> ln || f.cursor.column < wid
  (* unscrolled line *)
  then get_line_text f ln
    |> fun s -> if String.length s > wid then String.sub s 0 wid else s
  (* scrolled line *)
  else get_line_text f ln
    |> fun s -> String.sub s (f.cursor.column - wid) wid

(* [get_lines f l1 l2 wid] shows the horizontally-scrolled lines from
 * [l1] to [l2]. [l1] is included, [l2] is not. [wid] is the max length of
 * each line. *)
let rec get_lines f l1 l2 wid =
  if l1 >= l2 || l1 >= Array.length f.line_lengths then ""
  else get_scrolled_line_text f l1 wid ^ get_lines f (l1+1) l2 wid

(* [get_scrolled_lines st w h] displays the currently scrolled to lines,
 * so that the cursor is viewable horizontally and the first line displayed
 * is the current scroll line. [w] is the max width of each line,
 * and [h] is the max number of lines. *)
let get_scrolled_lines f w h =
  get_lines f (f.scroll_line_num) (f.scroll_line_num + h) w

(* [get_all_text f] returns a string representing all of the text in [f] *)
let get_all_text f = Rope.to_string f.contents

(* [start_selecting f] sets the fixed selecting point to the current
 * location of the cursor in [f]. *)
let start_selecting f = {f with selectpoint = Some f.cursor}

(* [select_text f l1 l2] selects text from [l1] to [l2].
 * This function forces [l1] and [l2] to be in order and in bounds.
 * The selection point is set to [l1] and the cursor is set to [l2]. *)
let select_text f l1 l2 =
  let (l1', l2') = make_range_valid (l1, l2) (cont_length f) in
  (* print_endline (string_of_int l1);
  print_endline (string_of_int l2); *)
  {f with
    cursor = get_location f.line_lengths f.cursor (l2'-1);
    selectpoint = Some (get_location f.line_lengths f.cursor l1');
  }

(* Returns [f] with no selected text. *)
let unselect_text f = {f with selectpoint = None}

(* [get_selected_range f] returns [None] if no text is selected,
 * or [Some (i1, i2)] if there is currently text selected from
 * index [i1] to [i2]. *)
let get_selected_range f =
  match f.selectpoint with
  | None -> None
  | Some sp -> (make_range_valid (f.cursor.index, sp.index) (cont_length f))
    |> fun (i0, i1) -> Some (i0, i1+1)

(* [get_select_start f] returns [Some (i, l, c)] where [i]
 * is the index of the beginning of the selection region, [l] is the line
 * number, and [c] is the column. If no selection has been made,
 * returns None. *)
let get_select_start f =
  match f.selectpoint with
  | None -> None
  | Some sp ->
    if sp.index < f.cursor.index then Some (sp.index, sp.line_num, sp.column)
    else Some (f.cursor.index, f.cursor.line_num, f.cursor.column)

(* [get_select_point f] returns [Some (i, l, c)] where [i]
 * is the index of the fixed selection point, [l] is the line number,
 * and [c] is the column. If no selection has been made, returns [None]. *)
let get_select_point f =
  match f.selectpoint with
  | Some {index=i; line_num=l; column=c} -> Some (i, l, c)
  | None -> None

(* [concat_with_newline ropes] concatenates a list of ropes
 * and appends a newline character to the end if it doesn't exist. *)
let concat_with_newline ropes =
  let combo = Rope.concat Rope.empty ropes in
  if (Rope.length combo = 0) || Rope.get combo (Rope.length combo - 1) <> '\n'
  then Rope.concat2 combo (Rope.of_string "\n")
  else combo

(* [insert_text f s] inserts string [s] into the contents
 * of [f] at location [l]. The beginning of the inserted string
 * will be at index [l]. If [l] is an invalid location, the closest
 * valid location will be used. If text append causes there to be
 * no newline character at the end of the contents, a newline is added. *)
let insert_text f s l' =
  let len_rope = cont_length f in
  let l = if l' < 0 then 0 else if l' > len_rope then len_rope else l' in
  let begin_rope = Rope.sub f.contents 0 l in
  let end_rope = Rope.sub f.contents l (len_rope - l) in
  let insert_rope = Rope.of_string s in
  let new_rope = concat_with_newline [begin_rope; insert_rope; end_rope] in
  (* print_endline s; *)
  let nf = add_undo f in
  { nf with
    contents = new_rope;
    line_lengths = line_lengths_arr new_rope;
    was_saved = false;
    num_redos = 0;
    redo_list = [];
  }

(* [insert_char f c] inserts a character [c] into the contents of [f]
 * at the cursor location in [f] and moves the cursor one character
 * to the right. *)
let insert_char f c =
  let len_rope = cont_length f in
  let l = f.cursor.index in
  let begin_rope = Rope.sub f.contents 0 l in
  let end_rope = Rope.sub f.contents l (len_rope - l) in
  let insert_rope = String.make 1 c |> Rope.of_string in
  let new_rope = Rope.concat Rope.empty [begin_rope; insert_rope; end_rope] in
  let nf = add_undo f in
  { nf with
    contents = new_rope;
    line_lengths = begin
      if c <> '\n' then
        let prev_len = Array.get f.line_lengths f.cursor.line_num in
        let new_lls = Array.copy f.line_lengths in
        Array.set new_lls f.cursor.line_num (prev_len + 1);
        new_lls
      else
        let len_arr = Array.length f.line_lengths in
        let line_len = Array.get f.line_lengths f.cursor.line_num in
        Array.concat [
          if f.cursor.line_num > 0
          then (Array.sub f.line_lengths 0 f.cursor.line_num)
          else [||];
          [|f.cursor.column + 1|];
          [|line_len - f.cursor.column|];
          if f.cursor.line_num < len_arr - 1
          then let ln = f.cursor.line_num + 1 in
            Array.sub f.line_lengths ln (len_arr - ln)
          else [||];
        ]
      end;
    was_saved = false;
    num_redos = 0;
    redo_list = [];
  } |> cursor_right

  let insert_scroll f c w h =
    insert_char f c |> fun f -> scroll f w h

(* [delete_text l1 l2] deletes all text in [f] from location
 * [l1] to [l2]. The new file contents contains everything up
 * to and not including [l1] and everything including [l2]
 * up to the end. [l1] and [l2] are automatically forced by
 * this function to be in bounds and in order. *)
let delete_text f l1' l2' =
  let (l1, l2) = make_range_valid (l1', l2') (cont_length f) in
  let begin_rope = Rope.sub f.contents 0 l1 in
  let len_rope = cont_length f in
  let end_rope = Rope.sub f.contents l2 (len_rope - l2) in
  let new_rope = concat_with_newline [begin_rope; end_rope] in
  let new_lls = line_lengths_arr new_rope in
  let nf = add_undo f in
  { nf with
    contents = new_rope;
    line_lengths = new_lls;
    was_saved = false;
    num_redos = 0;
    redo_list = [];
    cursor = get_location nf.line_lengths nf.cursor l1
  }

(* [delete_char f] deletes the character directly to the left of the
 * cursor in [f] and moves the cursor left one character. If there
 * is no character before the cursor, the file is left unchanged. *)
let delete_char f =
  if f.cursor.index = 0 then f else
  let deleted_char = Rope.get f.contents (f.cursor.index - 1) in
  let begin_rope = Rope.sub f.contents 0 (f.cursor.index - 1) in
  let len_rope = Rope.length f.contents in
  let end_rope = Rope.sub f.contents f.cursor.index
    (len_rope - f.cursor.index) in
  let new_rope = Rope.concat2 begin_rope end_rope in
  let nf = add_undo f in
  { nf with
    contents = new_rope;
    line_lengths = begin
      if deleted_char <> '\n' then
        let prev_len = Array.get f.line_lengths f.cursor.line_num in
        let new_lls = Array.copy f.line_lengths in
        Array.set new_lls f.cursor.line_num (prev_len - 1);
        new_lls
      else
        let len_arr = Array.length f.line_lengths in
        let p_line_len = Array.get f.line_lengths (f.cursor.line_num - 1) in
        let line_len = Array.get f.line_lengths f.cursor.line_num in
        Array.concat [
          Array.sub f.line_lengths 0 (f.cursor.line_num - 1);
          [|p_line_len + line_len - 1|];
          if f.cursor.line_num < len_arr - 1
          then let ln = f.cursor.line_num + 1 in
            Array.sub f.line_lengths ln (len_arr - ln)
          else [||];
        ]
      end;
    cursor = {
      index = f.cursor.index - 1;
      line_num =
        if deleted_char <> '\n' then f.cursor.line_num
        else f.cursor.line_num - 1;
      column =
        if deleted_char <> '\n' then f.cursor.column - 1
        else Array.get f.line_lengths (f.cursor.line_num - 1) - 1;
    };
    was_saved = false;
    num_redos = 0;
    redo_list = [];
  }

  let delete_scroll f w h =
    delete_char f |> fun f -> scroll f w h
(* [undo f] undoes the last change recorded in [f]. If there
 * is nothing left to undo, [undo f] will return [f] unchanged. *)
let undo f =
  let redo_file = add_redo f in
  match f.undo_list with
  | [] -> f
  | h::t -> {h with num_redos = redo_file.num_redos;
                    redo_list = redo_file.redo_list;
            }

(* [redo f] redoes the last change that was undone in [f]. If there
 * is nothing left to redo, [redo f] will return [f] unchanged. *)
let redo f =
  let undo_file = add_undo f in
  match f.redo_list with
  | [] -> f
  | h::t -> {h with num_undos = undo_file.num_undos;
                    undo_list = undo_file.undo_list;
            }

(* [color_text f lst] returns a copy of [f] with the color mappings of [lst] *)
let color_text f lst = {f with color_mapping = lst}

(* [get_coloring f] gets the coloring scheme of [f]. *)
let get_coloring f = f.color_mapping

(* [get_search_term f] gets the current search term in [f]. *)
let get_search_term f = f.search_term

(* [select_search_term f] returns an updated version of [f] with
 * with the next instance of the search term selected. The next instance is
 * defined as from the currently selected text. If no text is selected the
 * new version of [f] will have the first instance of its search term selected.
 * If there is no search term or it is not found, returns [f] with no text
 * selected *)
let rec select_search_term f =
  match get_selected_range f with
  | None ->
    begin
      try begin
        match f.search_term with
        | Some term ->
          begin
            let next_loc = Rope.search_forward_string term f.contents 0 in
            let next_loc_end = next_loc + (String.length term) in
            select_text f next_loc next_loc_end
          end
        | None -> {f with selectpoint = None;}
      end
      with
      | Not_found -> f
    end
  | Some (curr, _) ->
    begin
      try begin
        match f.search_term with
        | Some term ->
          begin
            let next_loc = Rope.search_forward_string term f.contents (curr+1) in
            let next_loc_end = next_loc + (String.length term) in
            select_text f next_loc next_loc_end
          end
        | None -> {f with selectpoint = None;}
      end
      with
      | Not_found -> select_search_term {f with selectpoint = None;}
    end

(* [find f s] updates [f] so that it holds [s] as its current
 * search term.  Unless [s] = "" or "\n",
 * for which it sets the term to [None] *)
let find f s =
  match s with
  | ""
  | "\n" -> { f with search_term = None; }
  | term -> { f with search_term = Some term; }

(* [remove_search_term f] removes the search_term of file [f] *)
let remove_search_term f = { f with search_term = None; }

(* [set_replace_term f s] sets the replace term of file [f] to [Some s] *)
let set_replace_term f s = { f with replace_term = Some s; }

(* [remove_replace_term f] sets the replace term of file [f] to [None]*)
let remove_replace_term f = { f with replace_term = None; }

(* [get_replace_term f] returns [Some s] where [r] is the replacement term
 * if the is no replacement term returns [None] *)
let get_replace_term f = f.replace_term

(* [replace_next f] returns an updated copy of [f] where the next instance
 * of the search term is replaced by the replace term, which is now selected
 * in the file. The next instance is
 * defined as from the currently selected text. If no text is selected the
 * new version of [f] will replace the first instance of its search term.
 * If there is no instance of the search term or there is no replace term,
 * the returned file will have the same text and no text selected *)
let replace_next f =
  match f.replace_term with
  | None -> {f with selectpoint = None;}
  | Some rep_term ->
    let to_replace = select_search_term f in
    match get_selected_range to_replace with
    | None -> to_replace
    | Some (st, en) ->
      begin
        let undo_file = add_undo f in
        let nf = delete_text to_replace st en in
        let nf = insert_text nf rep_term st in
        {(select_text nf st (st + String.length rep_term)) with
                 num_undos = undo_file.num_undos;
                 undo_list = undo_file.undo_list;
                 num_redos = 0;
                 redo_list = [];
        }
      end

  (* [replace_all_helper f] returns an updated copy of [f] where the every instance
   * of the search term is replaced by the replace term.
   * If there is no instance of the search term or either the search or replace
   * term does not exist, returns [f] with no text selected *)
  let rec replace_all_helper f =
    match f.replace_term with
    | None -> f
    | Some rep_term ->
      let to_replace = select_search_term f in
      match get_selected_range to_replace with
      | None -> to_replace
      | Some (st, en) ->
        begin
          let nf = delete_text to_replace st en in
          let nf = insert_text nf rep_term st in
          replace_all_helper (select_text nf st (st + String.length rep_term))
        end

(* [replace_all f] uses [replace_all_helper f] to replace all of the instances of
 * the seach term in [f] with the replace term. The use of the helper function allows
 * the user to undo/redo the full replacement rather than one at a time *)
let replace_all f =
  let undo_file = add_undo f in
  let replace_file = replace_all_helper f in
  { replace_file with
    num_undos = undo_file.num_undos;
    undo_list = undo_file.undo_list;
    num_redos = 0;
    redo_list = [];
  }


let first_index_of_line f linenum =
  Array.fold_left (+) 0 (Array.sub (line_lengths_arr f.contents) 0 linenum)

let last_index_of_line f linenum =
  (Array.fold_left (+) 0 (Array.sub (line_lengths_arr f.contents) 0 (linenum+1))) - 1

let get_visible_text f numlines =
  if numlines > List.length (get_line_lengths f) then get_all_text f
  else
  let before_lines =  Array.sub (line_lengths_arr f.contents)
      0 (f.scroll_line_num) in
  let start_ind =  Array.fold_left (+) 0 before_lines in
  if f.scroll_line_num + numlines > (List.length (get_line_lengths f)) then
    let end_ind  = Array.fold_left (+) 0 (line_lengths_arr f.contents) in
    get_text f start_ind end_ind else
  let visible_lines = Array.sub (line_lengths_arr f.contents)
      (f.scroll_line_num) (f.scroll_line_num + numlines) in
  let end_ind = (Array.fold_left (+) 0 visible_lines) + start_ind in
  get_text f start_ind end_ind
