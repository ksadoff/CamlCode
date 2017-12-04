open Lwt
open State
open LTerm_style
open LTerm_ui
open LTerm_geom
open LTerm_draw

let sty = {bold = None; underline = None; blink = None; reverse = None;
  foreground = Some white; background = None}

let cursor_style = {bold = None; underline = None; blink = None; reverse = None;
  foreground = Some black; background = Some white}

(* [draw_tabs st ctx] draws the tabs in state [st] at the top of context
 * [ctx]. *)
let draw_tabs st ctx =
  let num_tabs = get_file_names st |> List.length in
  let tab_height = (size ctx).rows in
  for n = 0 to (num_tabs-1) do
    draw_frame ctx
      {row1 = 0; col1 = 0 + (n*10); row2 = tab_height; col2 = 10 + (n*10)}
      Light
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
    |> draw_string_aligned ctx 0 H_align_center ~style:sty;

    (* contents of file *)
    get_all_text st |> draw_string txt_ctx 0 0 ~style:sty;

    (* cursor *)
    if (get_typing_area st) = File then
      let cursor_loc = get_cursor_location st in
      get_text st cursor_loc (cursor_loc+1)
      |> fun s -> (if s = "\n" then " " else s)
      |> draw_string txt_ctx (get_cursor_line_num st) (get_cursor_column st)
        ~style: cursor_style
    else ()
  end

  (* default display if no file open *)
  else begin
    draw_string_aligned ctx 10 H_align_center
      ~style:sty "Welcome to CamlCode"
  end
(* returns the value represented by an option if it exists, assumes not [None] *)
let extract = function
  | None -> failwith "not used"
  | Some s -> s

let draw_cmd st ctx =
  (* draw horizontal line *)
  draw_hline ctx 0 0 (size ctx).cols Heavy;
  (* label for command terminal *)
  draw_string_aligned ctx 0 H_align_center ~style:sty "Command Terminal";
  let cmd_in = st |> get_command_in |> extract in
  let cmd_out = st |> get_command_out |> extract in
  (* boxes for terminal input/output *)
  draw_frame ctx {row1 = 1; col1 = 0; row2 = 4; col2 = (size ctx).cols} Light;
  draw_frame ctx {row1 = 4; col1 = 0; row2 = 7; col2 = (size ctx).cols} Light;
  (* display terminal input/output *)
  draw_string ctx 2 2 ~style:sty (cmd_out);
  draw_string ctx 5 2 ~style:sty (cmd_in);
  if (get_typing_area st) = Command then
    draw_string ctx 5 ((cmd_in |> String.length)+2) ~style:cursor_style " "
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
