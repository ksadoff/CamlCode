open Lwt
open State
open LTerm_style
open LTerm_ui
open LTerm_geom
open LTerm_draw
open Str

let normal = {bold = None; underline = None; blink = None; reverse = None;
           foreground = Some white; background = None}

let highlighted = {bold = None; underline = None; blink = None; reverse = None;
                    foreground = Some black; background = Some white}

let get_tab_name str =
  (* if (String.sub str ((String.length str)-5) ((String.length str)-1)) =".txt"
  then let () = print_endline (String.sub str ((String.length str)-5) ((String.length str)-1)) in
  String.sub str 0 ((String.length str)-5)
  else str *)
  let file_name = Str.regexp "/[A-Za-z0-9]+[.][a-z]+\\b" in
  let find = Str.search_forward file_name str 0 in
  let full_name = Str.matched_string str in
  let without_ext = String.sub full_name 1 ((String.length full_name)-5) in
  if String.length without_ext > 7 then
    (String.sub without_ext 0 5)^"..." else
  without_ext

(* [draw_tabs st ctx] draws the tabs in state [st] at the top of context
 * [ctx]. *)
let draw_tabs st ctx =
  let num_tabs = get_file_names st |> List.length in
  let tab_height = (size ctx).rows in
  for n = 0 to (num_tabs-1) do
    draw_frame ctx
      {row1 = 0; col1 = 0 + (n*10); row2 = tab_height; col2 = 10 + (n*10)}
      Light;
    let file_name = (List.nth (get_file_names st) n) in
    if (String.length file_name)>10 then let tab_name = get_tab_name (file_name)
in
draw_string ctx 1 (1+(n*10)) tab_name ~style:normal;
    else let () =
           draw_string ctx 1 (1+(n*10)) file_name ~style:normal in ()
  done

(* [draw_file st ctx] draws the currently selected file in [st]
 * on context [ctx]. *)
let draw_file st ctx =
  (* draw horizontal line *)
  draw_hline ctx 0 0 (size ctx).cols Heavy;

let txt_ctx = sub ctx {row1=1; col1=0; row2=(size ctx).rows;
              col2=(size ctx).cols} in

  (* if file is open *)
  if is_on_file st then begin

    (* name of file at the top *)
    get_current_file_name st
    |> draw_string_aligned ctx 0 H_align_center ~style:normal;

    (* contents of file *)
    get_all_text st |> draw_string txt_ctx 0 0 ~style:normal;

    (* highlighted text *)
    match get_select_start st, get_selected_range st with
    | Some (_, l, c), Some (i0, i1) ->
      get_text st i0 i1
      |> Str.global_replace (Str.regexp "\n") " \n"
      |> draw_string txt_ctx l c ~style:highlighted
    | _ -> ();

    (* cursor *)
    if (get_typing_area st) = File then
      let cursor_loc = get_cursor_location st in
      get_text st cursor_loc (cursor_loc+1)
      |> fun s -> (if s = "\n" then " " else s)
      |> draw_string txt_ctx (get_cursor_line_num st) (get_cursor_column st)
        ~style: highlighted
  end

  (* default display if no file open *)
  else begin
    draw_string_aligned ctx 10 H_align_center
      ~style:normal "Welcome to CamlCode"
  end
(* returns the value represented by an option if it exists, assumes not [None] *)
let extract = function
  | None -> failwith "not used"
  | Some s -> s

let draw_cmd st ctx =
  (* draw horizontal line *)
  draw_hline ctx 0 0 (size ctx).cols Heavy;
  (* label for command terminal *)
  draw_string_aligned ctx 0 H_align_center ~style:normal "Command Terminal";
  let cmd_in = st |> get_command_in |> extract in
  let cmd_out = st |> get_command_out |> extract in
  (* boxes for terminal input/output *)
  draw_frame ctx {row1 = 1; col1 = 0; row2 = 4; col2 = (size ctx).cols} Light;
  draw_frame ctx {row1 = 4; col1 = 0; row2 = 7; col2 = (size ctx).cols} Light;
  (* display terminal input/output *)
  draw_string ctx 2 1 ~style:normal (cmd_out);
  draw_string ctx 5 1 ~style:normal (cmd_in);
  if (get_typing_area st) = Command then
    draw_string ctx 5 ((get_cmd_cursor st)+1) ~style:highlighted (get_cmd_text st)
  else ()


(* [draw_all ctx st] draws the state [st] on context [ctx]. *)
let draw_all ctx st =
  let size = size ctx in

  (* Clear context *)
  clear ctx;

  (* constants *)
  let tab_height = 3 in
  match get_command_out st with
  | Some _ ->
    begin
      (* Draw file in its own subcontext *)
      sub ctx {row1=tab_height+7; col1=0; row2=size.rows; col2=size.cols}
      |> draw_file st;

      (* Draw tabs *)
      sub ctx {row1=0; col1=0; row2=tab_height; col2=size.cols}
      |> draw_tabs st;

      (* draw command prompt *)
      sub ctx {row1=tab_height; col1=0; row2=tab_height+7; col2=size.cols}
      |> draw_cmd st
    end
  | None ->
    begin
      (* Draw file in its own subcontext *)
      sub ctx {row1=tab_height; col1=0; row2=size.rows; col2=size.cols}
      |> draw_file st;

      (* Draw tabs *)
      sub ctx {row1=0; col1=0; row2=tab_height; col2=size.cols}
      |> draw_tabs st;
    end

(* [draw] is called by [repl] in command.ml after user input is received.
 * It takes in the current state, updates the user interface to reflect any
 * change the user made to the state, and returns a unit. *)
let draw term stref =
  LTerm_ui.create term (fun ui matrix ->
    let size = LTerm_ui.size ui in
    let ctx = context matrix size in
    draw_all ctx !stref
  )
