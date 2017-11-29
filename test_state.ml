open OUnit2
open State

let slstate = empty_state |> fun st -> open_file st "testtxts/somelines.txt"

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
      save_file fghij_state "testtxts/statetemp.txt";
      open_file fghij_state "testtxts/statetemp.txt" |> get_all_text
    )
  );

  (* delete text *)
  "delete" >:: (fun _ -> assert_equal "h" 
    (delete_text slstate 1 17 |> get_all_text));

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
]