open CamomileLibraryDyn.Camomile
open Lwt
open LTerm_widget

(* let update num_open_files state *)
let main () =
  let waiter, wakener = wait () in

  let vbox = new LTerm_widget.vbox in
  let frame = new LTerm_widget.frame in
  let editor = new LTerm_edit.edit () in
  let vscroll = new LTerm_widget.vscrollbar ~width:1 editor#vscroll in
  frame#set editor;

  let create_tab n = new button ("file "^ (string_of_int n)) in
  let tabs = Array.init 3 create_tab in
  let tab_labels = (new label "") in

  vbox#add ~expand:false (new hline);
  (* let add_tabs = *)
    for i = 0 to 0 do
      let hbox = new hbox in
      hbox#add ~expand:false (new vline);
      hbox#add tabs.(i);
      hbox#add ~expand:false (new vline);
      hbox#add tabs.(i+1);
      hbox#add ~expand:false (new vline);
      vbox#add ~expand:false hbox;
    done;
(* in
  add_tabs; *)

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
  ");

  Lazy.force LTerm.stdout
  >>= fun term ->
  LTerm.enable_mouse term
  >>= fun () ->
  Lwt.finalize
    (fun () -> LTerm_widget.run term vbox waiter)
    (fun () -> LTerm.disable_mouse term)

let () = Lwt_main.run (main ())

(* let update (st: State.state) = main () *)
