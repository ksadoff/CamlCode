open Lwt
open State
open LTerm_style
open LTerm_ui
open LTerm_geom
open LTerm_draw

let sty = {bold = None; underline = None; blink = None; reverse = None;
  foreground = Some white; background = None}

(* [draw_tabs ctx st] draws the tabs in state [st] at the top of context 
 * [ctx]. *)
let draw_tabs ctx st = 
  let num_tabs = get_file_names st |> List.length in
  let tab_height = (size ctx).rows in
  for n = 0 to (num_tabs-1) do
    draw_frame ctx 
      {row1 = 0; col1 = 0 + (n*10); row2 = tab_height; col2 = 10 + (n*10)} 
      Light
  done

(* [draw_file ctx st] draws the currently selected file in [st]
 * on context [ctx]. *)
let draw_file ctx st =
  (* draw horizontal line *)
  draw_hline ctx 0 0 (size ctx).cols Heavy;

  (* if file is open *)
  if is_on_file st then begin
    (* name of file at the top *)
    get_current_file_name st 
      |> draw_string_aligned ctx 0 H_align_center ~style:sty;
    (* contents of file *)
    get_all_text st |> draw_string ctx 1 0 ~style:sty
  end

  (* default display if no file open *)
  else begin
    draw_string_aligned ctx 10 H_align_center 
      ~style:sty "Welcome to CamlCode"
  end

(* [draw_all ctx st] draws the state [st] on context [ctx]. *)
let draw_all ctx st =
  let size = size ctx in

  (* Clear context *)
  clear ctx;

  (* constants *)
  let tab_height = 3 in

  (* Draw file in its own subcontext *)
  let filectx = sub ctx 
    {row1=tab_height; col1=0; row2=size.rows; col2=size.cols} in
  draw_file filectx st;

  (* Draw tabs *)
  let tabctx = sub ctx {row1=0; col1=0; row2=tab_height; col2=size.cols} in
  draw_tabs tabctx st

(* [draw] is called by [repl] in command.ml after user input is received. 
 * It takes in the current state, updates the user interface to reflect any 
 * change the user made to the state, and returns a unit. *)
let draw term st = 
  LTerm_ui.create term (fun ui matrix -> 
    let size = LTerm_ui.size ui in
    let ctx = context matrix size in
    draw_all ctx st
  )
