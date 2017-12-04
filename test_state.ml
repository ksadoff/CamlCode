open OUnit2
open State

let slstate = empty_state |> fun st -> open_file st "testtxts/somelines.txt"
let slstate' = select_text slstate 0 5

(* If [f st] returns [st'], this function returns a test case with
 * name [tname] that checks that [(get_cursor_location st', *)
let basic_clipboard = string_to_clipboard "hello"
let basic_state = empty_state |> fun st -> open_file st "testtxts/clipboardtest.txt"
let basic_state_file = File.select_text (get_current_file basic_state) 0 9
let basic_state' = change_selected_file (File.get_name basic_state_file) basic_state

let basic_state_paste = move_cursor basic_state 4
let paste_text = "aaaaaaaaaaaaaaaaaaaa\n\nhi"
(* let basic_state_paste = change_selected_file (File.get_name basic_state_paste_file) basic_state' *)

(* If [f st] returns [st'], this function returns a test case with
 * name [tname] that checks that [(get_cursor_location st',
 * get_cursor_line_num st', get_cursor_column st')] equals [exp]. *)
let test_cursor tname f st exp =
  let st' = f st in
  let inc = (get_cursor_location st', get_cursor_line_num st',
    get_cursor_column st') in
  tname >:: (fun _ -> assert_equal exp inc)

(* Test cases for File module. *)
let tests = [
  (* open and read file *)
  "read_somelines" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    (slstate |> get_all_text)
    ~printer: (fun x -> x)
  );

  (* get_text *)
  "read_somelines" >:: (fun _ -> assert_equal "hello"
    (slstate |> fun st -> get_text st 0 5)
    ~printer: (fun x -> x)
  );

  (* new file, modify file, save file *)
  "save" >:: (fun _ -> assert_equal "fghij\n" (
      new_file "testtxts/statetest.txt";
      let fghij_state = empty_state
      |> fun st -> open_file st "testtxts/statetest.txt"
      |> fun st -> insert_text st "fghij" 0 in
      save_file fghij_state "testtxts/statetemp.txt" |> fun st -> ();
      open_file fghij_state "testtxts/statetemp.txt" |> get_all_text
    )
  );

  (* insert char *)
  "ins_char" >:: (fun _ -> assert_equal "hello\nworld\na\n!!!\n" (
    move_cursor slstate 12
    |> fun st -> insert_char st 'a'
    |> get_all_text
  ));

  (* delete text *)
  "delete" >:: (fun _ -> assert_equal "h\n"
    (delete_text slstate 1 17 |> get_all_text));

  (* delete char *)
  "del_char" >:: (fun _ -> assert_equal "hello\nworld\n!!!\n" (
    move_cursor slstate 12
    |> delete_char
    |> get_all_text
  ));

  (* test cursor *)
  test_cursor "cursor0" (fun x -> x) slstate (0, 0, 0);
  test_cursor "cursor1" (fun st -> move_cursor st 8) slstate (8, 1, 2);
  test_cursor "cursor2" (fun st -> move_cursor st 9 |> cursor_left)
    slstate (8, 1, 2);
  test_cursor "cursor3" cursor_right slstate (1, 0, 1);
  test_cursor "cursor4" (fun st -> move_cursor st 8 |> cursor_up)
    slstate (2, 0, 2);
  test_cursor "cursor5" cursor_down slstate (6, 1, 0);

  (* scrolling *)
  "scroll" >:: (fun _ -> assert_equal 2
    (scroll_to slstate 2 |> get_scroll_line));

  (* test selection *)
  "select" >:: (fun _ -> assert_equal (Some (3, 9))
    (select_text slstate 3 9 |> get_selected_range));
  "unselect" >:: (fun _ -> assert_equal None
    (select_text slstate 3 9 |> unselect_text |> get_selected_range));

  "unselect" >:: (fun _ -> assert_equal None
                     (select_text slstate 3 9 |> unselect_text |> get_selected_range));

  (*clipboard*)
  "selected range" >:: (fun _ -> assert_equal (Some (0, 9)) (get_selected_range (select_text slstate 0 9)));
  "clipboard empty" >:: (fun _ -> assert_equal (string_to_clipboard "") (new_clipboard));
  "clipboard copy" >:: (fun _ -> assert_equal basic_clipboard (copy slstate' |>
                                                               get_clipboard));
  (* "clipboard paste" >:: (fun _ -> assert_equal (paste_text)
                            (paste basic_state_paste |> get_all_text)); *)

                            (* tests for setting and getting the search term of a file *)
  "find0" >:: (fun _ -> assert_equal (Some "hello")
    (get_search_term (find slstate ("hello"))));
  "find1" >:: (fun _ -> assert_equal (Some " ")
    (get_search_term (find slstate " ")));
  "find2" >:: (fun _ -> assert_equal None
    (get_search_term (find slstate "")));
  "find3" >:: (fun _ -> assert_equal None
    (get_search_term slstate));

  (* tests for selecting the search term of a file *)
  "sel_search0" >:: (fun _ -> assert_equal (Some (0,1))
    ((find slstate "h") |> select_search_term |> get_selected_range));
  (* loop back to start at end of file *)
  "sel_search1" >:: (fun _ -> assert_equal (Some (0,1))
    ((find slstate "h") |> select_search_term |> select_search_term |> get_selected_range));
  (* term not found *)
  "sel_search2" >:: (fun _ -> assert_equal None
    ((find slstate "Hello") |> select_search_term |> get_selected_range));
  (* first location *)
  "sel_search3" >:: (fun _ -> assert_equal (Some (2,3))
    ((find slstate "l") |> select_search_term |> get_selected_range));
  (* second location *)
  "sel_search4" >:: (fun _ -> assert_equal (Some (3,4))
    ((find slstate "l") |> select_search_term |> select_search_term |> get_selected_range));
  (* third location *)
  "sel_search5" >:: (fun _ -> assert_equal (Some (9,10))
    ((find slstate "l") |> select_search_term |> select_search_term |> select_search_term |> get_selected_range));

  (* tests for removing the search term of a file *)
  "rem_find0" >:: (fun _ -> assert_equal None
    ((find slstate "hello") |> remove_search_term |> get_search_term));
  "rem_find1" >:: (fun _ -> assert_equal None
    ((find slstate " ") |> remove_search_term |> get_search_term));

  (* tests for setting, getting, and removing the replace term of a file *)
  "rep0" >:: (fun _ -> assert_equal None
    (slstate |> get_replace_term));
  "rep1" >:: (fun _ -> assert_equal (Some "H")
    ((set_replace_term slstate "H") |> get_replace_term));
  "rep2" >:: (fun _ -> assert_equal None
    ((set_replace_term slstate "H") |> remove_replace_term |> get_replace_term));
  "rep3" >:: (fun _ -> assert_equal (Some "")
    ((set_replace_term slstate "") |> get_replace_term));

  (* tests for replacing the next search term *)
  "rep_next0" >:: (fun _ -> assert_equal "Hello\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "h") "H") |> replace_next |> get_all_text));
  "rep_next5" >:: (fun _ -> assert_equal (Some (0,1))
    ((set_replace_term (find slstate "h") "H") |> replace_next |> get_selected_range));
  "rep_next1" >:: (fun _ -> assert_equal "Hello\nHorld\n\n!!!\n"
    ((find ((set_replace_term (find slstate "h") "H") |> replace_next) "w") |> replace_next |> get_all_text));
  "rep_next2" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "\n") "") |> replace_next |> replace_next |> replace_next |> replace_next |> get_all_text));
  "rep_next3" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "") "H") |> replace_next |> get_all_text));
  "rep_next4" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((find slstate "h") |> replace_next |> get_all_text));

  (* tests for replace all *)
   "rep_all0" >:: (fun _ -> assert_equal "Hello\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "h") "H") |> replace_all |> get_all_text));
  "rep_all1" >:: (fun _ -> assert_equal "heLLo\nworLd\n\n!!!\n"
    ((set_replace_term (find slstate "l") "L") |> replace_all |> get_all_text));
  "rep_all2" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "\n") "") |> replace_all |> get_all_text));
  "rep_all3" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "H") "h") |> replace_all |> get_all_text));

  (* tests for undo *)
  "undo0" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((insert_text slstate "hi" 0) |> undo |> get_all_text));
  "undo1" >:: (fun _ -> assert_equal 0
    ((insert_text slstate "hi" 0) |> undo |> get_cursor_location));
  "undo2" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((delete_text slstate 0 5) |> undo |> get_all_text));
  "undo3" >:: (fun _ -> assert_equal 0
    ((delete_text slstate 0 5) |> undo |> get_cursor_location));
  "undo4" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((insert_char slstate 'a') |> undo |> get_all_text));
  "undo5" >:: (fun _ -> assert_equal 0
    ((insert_char slstate 'a') |> undo |> get_cursor_location));
  "undo6" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((delete_char slstate) |> undo |> get_all_text));
  "undo7" >:: (fun _ -> assert_equal 0
    ((delete_char slstate) |> undo |> get_cursor_location));
  "undo8" >:: (fun _ -> assert_equal "ello\nworld\n\n!!!\n"
    (slstate |> cursor_right |> delete_char |> cursor_right |> delete_char |> undo |> get_all_text));
  "undo9" >:: (fun _ -> assert_equal 1
    (slstate |> cursor_right |> delete_char |> cursor_right |> delete_char |> undo |> get_cursor_location));
  "undo10" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    (slstate |> cursor_right |> delete_char |> cursor_right |> delete_char |> undo |> undo |> get_all_text));
  "undo11" >:: (fun _ -> assert_equal 1
    (slstate |> cursor_right |> delete_char |> cursor_right |> delete_char |> undo |> undo |> get_cursor_location));
  "undo12" >:: (fun _ -> assert_equal "ello\nworld\n\n!!!\n"
    (slstate |> cursor_right |> delete_char |> undo |> delete_char |> get_all_text));
  "undo13" >:: (fun _ -> assert_equal 0
    (slstate |> cursor_right |> delete_char |> undo |> delete_char |> get_cursor_location));
  "undo14" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    (slstate |> undo |> get_all_text));
  "undo15" >:: (fun _ -> assert_equal 0
    (slstate |> undo |> get_cursor_location));
  "undo16" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "l") "L") |> replace_next |> undo |> get_all_text));
  "undo17" >:: (fun _ -> assert_equal 0
    ((set_replace_term (find slstate "l") "L") |> replace_next |> undo |> get_cursor_location));
  "undo18" >:: (fun _ -> assert_equal "heLlo\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "l") "L") |> replace_next |> replace_next |> undo |> get_all_text));
  "undo19" >:: (fun _ -> assert_equal 0
    ((set_replace_term (find slstate "l") "L") |> replace_next |> replace_next |> undo |> get_cursor_location));
  "undo20" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "l") "L") |> replace_all |> undo |> get_all_text));
  "undo21" >:: (fun _ -> assert_equal 0
    ((set_replace_term (find slstate "l") "L") |> replace_all |> undo |> get_cursor_location));

  (* tests for redo *)
  "redo0" >:: (fun _ -> assert_equal "hihello\nworld\n\n!!!\n"
    ((insert_text slstate "hi" 0) |> undo |> redo |> get_all_text));
  "redo1" >:: (fun _ -> assert_equal 0
    ((insert_text slstate "hi" 0) |> undo |> redo |> get_cursor_location));
  "redo2" >:: (fun _ -> assert_equal "\nworld\n\n!!!\n"
    ((delete_text slstate 0 5) |> undo |> redo |> get_all_text));
  "redo3" >:: (fun _ -> assert_equal 0
    ((delete_text slstate 0 5) |> undo |> redo |> get_cursor_location));
  "redo4" >:: (fun _ -> assert_equal "ahello\nworld\n\n!!!\n"
    ((insert_char slstate 'a') |> undo |> redo |> get_all_text));
  "redo5" >:: (fun _ -> assert_equal 1
    ((insert_char slstate 'a') |> undo |> redo |> get_cursor_location));
  "redo6" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((delete_char slstate) |> undo |> redo |> get_all_text));
  "redo7" >:: (fun _ -> assert_equal 0
    ((delete_char slstate) |> undo |> redo |> get_cursor_location));
  "redo8" >:: (fun _ -> assert_equal "llo\nworld\n\n!!!\n"
    (slstate |> cursor_right |> delete_char |> cursor_right |> delete_char |> undo |> redo |> get_all_text));
  "redo9" >:: (fun _ -> assert_equal 0
    (slstate |> cursor_right |> delete_char |> cursor_right |> delete_char |> undo |> redo |> get_cursor_location));
  "redo10" >:: (fun _ -> assert_equal "llo\nworld\n\n!!!\n"
    (slstate |> cursor_right |> delete_char |> cursor_right |> delete_char |> undo |> undo |> redo |> redo |> get_all_text));
  "redo11" >:: (fun _ -> assert_equal 0
    (slstate |> cursor_right |> delete_char |> cursor_right |> delete_char |> undo |> undo |> redo |> redo |> get_cursor_location));
  "redo12" >:: (fun _ -> assert_equal "ello\nworld\n\n!!!\n"
    (slstate |> cursor_right |> delete_char |> undo |> delete_char |> redo |> get_all_text));
  "redo13" >:: (fun _ -> assert_equal 0
    (slstate |> cursor_right |> delete_char |> undo |> delete_char |> redo |> get_cursor_location));
  "redo14" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    (slstate |> redo |> get_all_text));
  "redo15" >:: (fun _ -> assert_equal 0
    (slstate |> redo |> get_cursor_location));
  "redo16" >:: (fun _ -> assert_equal "heLlo\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "l") "L") |> replace_next |> undo |> redo |> get_all_text));
  "redo17" >:: (fun _ -> assert_equal 0
    ((set_replace_term (find slstate "l") "L") |> replace_next |> undo |> redo |> get_cursor_location));
  "redo18" >:: (fun _ -> assert_equal "heLLo\nworld\n\n!!!\n"
    ((set_replace_term (find slstate "l") "L") |> replace_next |> replace_next |> undo |> redo |> get_all_text));
  "redo19" >:: (fun _ -> assert_equal 0
    ((set_replace_term (find slstate "l") "L") |> replace_next |> replace_next |> undo |> redo |> get_cursor_location));
  "redo20" >:: (fun _ -> assert_equal "heLLo\nworLd\n\n!!!\n"
    ((set_replace_term (find slstate "l") "L") |> replace_all |> undo |> redo |> get_all_text));
  "redo21" >:: (fun _ -> assert_equal 0
    ((set_replace_term (find slstate "l") "L") |> replace_all |> undo |> redo |> get_cursor_location));
  "redo22" >:: (fun _ -> assert_equal "heLLo\nworLd\n\n!!!\n"
    ((set_replace_term (find slstate "l") "L") |> replace_all |> undo |> redo |> undo |> redo |> undo |> redo |> get_all_text));
  "redo23" >:: (fun _ -> assert_equal 0
    ((set_replace_term (find slstate "l") "L") |> replace_all |> undo |> redo |> undo |> redo |> undo |> redo |> get_cursor_location));

  (* command terminal tests *)
  "command0" >:: (fun _ -> assert_equal None
    (slstate |> get_command_out));
  "command1" >:: (fun _ -> assert_equal None
    (slstate |> get_command_in));
  "command2" >:: (fun _ -> assert_equal (Some "")
    (slstate |> open_terminal |> get_command_out));
  "command3" >:: (fun _ -> assert_equal (Some "")
    (slstate |> open_terminal |> get_command_in));
  "command4" >:: (fun _ -> assert_equal None
    (slstate |> open_terminal |> close_terminal |> get_command_out));
  "command5" >:: (fun _ -> assert_equal None
    (slstate |> open_terminal |> close_terminal |> get_command_in));
  "command6" >:: (fun _ -> assert_equal (Some "hi")
    ((set_command_out slstate "hi") |> get_command_out));
  "command7" >:: (fun _ -> assert_equal (Some "")
    ((set_command_out slstate "hi") |> get_command_in));
  "command8" >:: (fun _ -> assert_equal (Some "")
    ((set_command_in slstate "hi") |> get_command_out));
  "command9" >:: (fun _ -> assert_equal (Some "hi")
    ((set_command_in slstate "hi") |> get_command_in));
  "command10" >:: (fun _ -> assert_equal None
    ((set_command_in slstate "hi") |> close_terminal |> get_command_out));
  "command11" >:: (fun _ -> assert_equal None
    ((set_command_in slstate "hi") |> close_terminal |> get_command_in));
  "command12" >:: (fun _ -> assert_equal (Some "a")
    ((cmd_insert slstate 'a') |> get_command_in));
  "command13" >:: (fun _ -> assert_equal (Some "ab")
    (cmd_insert (cmd_insert slstate 'a') 'b' |> get_command_in));
  "command14" >:: (fun _ -> assert_equal (Some "")
    ((cmd_insert slstate 'a') |> cmd_delete |> get_command_in));
  "command15" >:: (fun _ -> assert_equal (Some "a")
    (cmd_insert (cmd_insert slstate 'a') 'b' |> cmd_delete |> get_command_in));
  "command16" >:: (fun _ -> assert_equal 0
    (slstate |>  get_cmd_cursor));
  "command17" >:: (fun _ -> assert_equal 0
    (slstate |> cmd_cursor_right |>  get_cmd_cursor));
  "command18" >:: (fun _ -> assert_equal 0
    (slstate |> cmd_cursor_left |>  get_cmd_cursor));
  "command19" >:: (fun _ -> assert_equal 1
    (cmd_insert slstate 'a' |> cmd_cursor_right |>  get_cmd_cursor));
  "command20" >:: (fun _ -> assert_equal 0
    (cmd_insert slstate 'a' |> cmd_cursor_left |>  get_cmd_cursor));
  "command21" >:: (fun _ -> assert_equal " "
    (cmd_insert slstate 'a' |> get_cmd_text));
  "command22" >:: (fun _ -> assert_equal " "
    (cmd_insert slstate 'a' |> cmd_delete |> get_cmd_text));
  "command23" >:: (fun _ -> assert_equal "a"
    (cmd_insert slstate 'a' |> cmd_cursor_left |> get_cmd_text));
  (* tests for reading and toggling the typing area *)
  "t_area0" >:: (fun _ -> assert_equal File (get_typing_area slstate));
  "t_area1" >:: (fun _ -> assert_equal Command (slstate |> toggle_typing_area |> get_typing_area));
  "t_area2" >:: (fun _ -> assert_equal File
                    (slstate |> toggle_typing_area |> toggle_typing_area |> get_typing_area));
]
