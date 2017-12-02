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



(* let main () =
  Lazy.force LTerm.stdout
  >>= fun term ->

  (* Coordinates of the message. *)
  let coord = ref { row = 0; col = 0 } in

  LTerm_ui.create term (fun matrix size -> draw matrix size !coord)
  >>= fun ui ->
  Lwt.finalize (fun () -> loop ui coord) (fun () -> LTerm_ui.quit ui)

let () = Lwt_main.run (main ()) *)
(* let main () = *)
  (* let waiter, wakener = wait () in

  let vbox = new LTerm_widget.vbox in
  let frame = new LTerm_widget.frame in
  let editor = new LTerm_edit.edit () in
  let vscroll = new LTerm_widget.vscrollbar ~width:1 editor#vscroll in
  frame#set editor;

  let create_tab n = new button ("file "^ (string_of_int n)) in
  let tabs = Array.init 6 create_tab in
  let tab_labels = (new label "") in

  vbox#add ~expand:false (new hline);

  let add_tabs =
  (* for n = 0 in (List.length get) *)
  (* let add_tabs = *)
    let hbox = new hbox in
    for n = 0 to ((Array.length tabs) - 1) do
      hbox#add ~expand:false (new vline);
      (* for i = 0 to 0 do *)
        hbox#add tabs.(n);
      (* hbox#add tabs.(i+1);
      hbox#add ~expand:false (new vline);
      hbox#add tabs.(i+2); *)
      (* hbox#add ~expand:false (new vline); *)
      (* hbox#add tabs.(i+3);
      hbox#add ~expand:false (new vline); *)
      (* hbox#add tabs.(i+4);
      hbox#add ~expand:false (new vline); *)
      (* hbox#add tabs.(i+5);
      hbox#add ~expand:false (new vline); *)
      done;
      hbox#add ~expand:false (new vline);
      vbox#add ~expand:false hbox;
  (* done; *)
in
  add_tabs;

  (* vbox#add ~expand:false (new hline); *)


  (* let sidebar = new LTerm_widget.vbox in
  sidebar#set_allocation {sidebar#allocation with col1=1; col2 =1};
  vbox#add sidebar; *)

  (* let label = new label "_" in
    vbox#add label; *)
  (*eventually have to have as many tabs as necessary (for loop/recursion?)
    for now just test it out*)


  (*layer 1 for file 1*)
(*
  for i = 0 to 2 do
        let hbox = new hbox in
    (* hbox#set_allocation {row1 = 1; col1= 0;row2=1;col2=0}; *)
    let button i =
      let button = new button ("button" ^ string_of_int i) in
      (* button#on_click (fun () -> label#set_text (string_of_int i)); *)
      (* button#set_allocation {row1=1; col1=0; row2=1; col2=0}; *)
      button
    in
    hbox#add (button (i + 1));
    hbox#add ~expand:false (new vline);
    (* hbox#add (button (i * 3 + 2));
    hbox#add ~expand:false (new vline);
    hbox#add (button (i * 3 + 3)); *)
    vbox#add ~expand:false (new hline);
    vbox#add hbox
  done; *)
  (* let tab1 = new LTerm_buttons_impl.button "file 1" in
  let tab2 = new LTerm_buttons_impl.button "file 2" in
  vbox#add tab1;
  vbox#add tab2; *)



  (* vbox#add (new t "glue") ; *)

  vbox#add frame;
  vbox#add ~expand:false vscroll;


(* Exit when the user presses ctl-q *)
  editor#bind
    (let open LTerm_key in
     [ { control = true; meta = false; shift = false
       ; code = Char (UChar.of_char 'q') }
   ;  ])
  [ LTerm_edit.Custom (fun () -> wakeup wakener ()) ];

  Zed_edit.insert editor#context
    (Zed_rope.of_string "\
  Welcome to CamlCode!
  [insert stuff about keybindings]
  Type Ctrl-q to exit.
  "); *)
(*
  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enable_mouse term
  >>= fun () ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term vbox waiter)
    (fun () -> LTerm.disable_mouse term) *)




(*let main () =
  let init_state = empty_state in
  update init_state*)
