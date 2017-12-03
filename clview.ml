(* open CamomileLibraryDyn.Camomile *)
open Lwt
open LTerm_widget
open LTerm_geom
open LTerm_text
open LTerm_key
open State
open LTerm_style
open CamomileLibrary
open LTerm_ui

(* type coord = LTerm_geom.coord *)

let draw_editor ui matrix coord =
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_frame_labelled ctx { row1 = 0; col1 = 0; row2 = size.rows; col2 = size.cols } ~alignment:H_align_center "CamlCode" LTerm_draw.Light;
  LTerm_draw.draw_frame ctx {row1 = 0; col1 = 0; row2 = 3; col2 = 10} LTerm_draw.Light

let sty = {bold = None; underline = None; blink = None; reverse = None;
           foreground = Some white; background = None}
           
let draw ui matrix coord=
  let size = LTerm_ui.size ui in
  let ctx = LTerm_draw.context matrix size in
  LTerm_draw.clear ctx;
  LTerm_draw.draw_frame_labelled ctx { row1 = 0; col1 = 0; row2 = size.rows; col2 = size.cols } ~alignment:H_align_center "CamlCode" LTerm_draw.Light;
  (* LTerm_draw.draw_hline ctx 0 0  *)
  (* LTerm_draw.draw_char ctx 10 10 ~style:sty (CamomileLibrary.UChar.of_char 'a'); *)
  LTerm_draw.draw_string_aligned ctx 10 H_align_center ~style:sty "Welcome to CamlCode";
  for n = 0 to 5 do
    LTerm_draw.draw_frame ctx {row1 = 0; col1 = 0 + (n*10); row2 = 3; col2 = 10 + (n*10)} LTerm_draw.Light
  done
