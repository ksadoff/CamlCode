open OUnit2
open File

let int_list_printer l = List.fold_left
  (fun acc x -> acc ^ (string_of_int x) ^ "; ") "" l
  |> fun s -> "[" ^ s ^ "]"

let somelines = File.open_file "testtxts/somelines.txt"
let somelines_moved = move_cursor somelines 12
let somelines_moved2 = move_cursor somelines 8

(* [move_cursor_test test_name f l exp] returns a test case where
 * the cursor in file [f] is moved to index [l]. [exp] is
 * the expected (index, line num, column) tuple. [test_name] is
 * name of the test, used by OUnit. *)
let move_cursor_test test_name orig_f l exp =
  let f = orig_f |> fun f' -> move_cursor f' l in
  test_name >:: (fun _ -> assert_equal exp
    (get_cursor_location f,
    get_cursor_line_num f,
    get_cursor_column f)
    ~printer: (fun (i,l,c) -> "(" ^ (string_of_int i) ^ ", " ^
      (string_of_int l) ^ ", " ^
      (string_of_int c) ^ ")")
  )

(* [modify_file_test test_name f ffun exp] returns a test case where
 * [f] is acted on by [ffun]. [exp] is the expected (index, line num,
 * column) tuple. [test_name] is name of the test, used by OUnit. *)
let modify_file_test test_name orig_f ffun exp =
  let f = ffun orig_f in
  test_name >:: (fun _ -> assert_equal exp
    (get_cursor_location f,
    get_cursor_line_num f,
    get_cursor_column f)
    ~printer: (fun (i,l,c) -> "(" ^ (string_of_int i) ^ ", " ^
      (string_of_int l) ^ ", " ^
      (string_of_int c) ^ ")")
  )

(* [insert_test test_name f s l exp_s exp_ls] creates a test case with name
 * [test_name] that inserts string [s] into the contents of [f] at
 * location [l]. [exp_s] is the expected string contents of [f], and
 * [exp_ls] is the expected list of line lengths in [f]. *)
let insert_test test_name orig_f s l exp_s exp_ls =
  let f = insert_text orig_f s l in
  test_name >:: (fun _ -> assert_equal (exp_s, exp_ls)
    (get_all_text f, get_line_lengths f)
    ~printer: (fun (s, ls) -> "\"" ^ s ^ "\", " ^ (int_list_printer ls))
  )

(* [ins_ch_test test_name f s l exp_s exp_ls] creates a test case with name
 * [test_name] that inserts char [c] into the contents of [f] at
 * location [l]. [exp_s] is the expected string contents of [f], and
 * [exp_ls] is the expected list of line lengths in [f]. *)
 let ins_ch_test test_name orig_f c l exp_s exp_ls =
  let f = let f' = move_cursor orig_f l in insert_char f' c in
  test_name >:: (fun _ -> assert_equal (exp_s, exp_ls)
    (get_all_text f, get_line_lengths f)
    ~printer: (fun (s, ls) -> "\"" ^ s ^ "\", " ^ (int_list_printer ls))
  )

(* [delete_test test_name f l1 l2 exp_s exp_ls] creates a test case with
 * name [test_name] that deletes contents of [f] from [l1] to [l2]. [exp_s]
 * is the expected string contents of [f], and [exp_ls] is the expected
 * list of line lengths in [f]. *)
 let delete_test test_name orig_f l1 l2 exp_s exp_ls =
  let f = delete_text orig_f l1 l2 in
  test_name >:: (fun _ -> assert_equal (exp_s, exp_ls)
    (get_all_text f, get_line_lengths f)
    ~printer: (fun (s, ls) -> "\"" ^ s ^ "\", " ^ (int_list_printer ls))
  )

(* Test cases for File module. *)
let tests = [
  "read_file" >:: (fun _ -> assert_equal "test file\n"
    (open_file "testtxts/easy.txt" |> get_all_text)
  );
  "read_somelines" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    (open_file "testtxts/somelines.txt" |> get_all_text)
    ~printer: (fun x -> x)
  );
  "line_lengths" >:: (fun _ -> assert_equal [6; 6; 1; 4]
    (somelines |> get_line_lengths)
    ~printer: int_list_printer);

  (* cursor moving tests *)
  move_cursor_test "cursor0" somelines 1 (1, 0, 1);
  move_cursor_test "cursor1" somelines 5 (5, 0, 5);
  move_cursor_test "cursor2" somelines 6 (6, 1, 0);
  move_cursor_test "cursor3" somelines 16 (16, 3, 3);
  move_cursor_test "cursor4" somelines 17 (16, 3, 3);
  move_cursor_test "cursor5" somelines 0 (0, 0, 0);
  move_cursor_test "cursor6" somelines (-1) (0, 0, 0);
  move_cursor_test "cursor7" somelines_moved 12 (12, 2, 0);
  move_cursor_test "cursor8" somelines_moved 13 (13, 3, 0);
  move_cursor_test "cursor9" somelines_moved 11 (11, 1, 5);
  move_cursor_test "cursor10" somelines_moved 0 (0, 0, 0);
  move_cursor_test "cursor11" somelines_moved 5 (5, 0, 5);
  move_cursor_test "cursor12" somelines_moved 6 (6, 1, 0);
  move_cursor_test "cursor13" somelines_moved2 11 (11, 1, 5);
  move_cursor_test "cursor14" somelines_moved2 6 (6, 1, 0);

  (* cursor moves by 1 tests *)
  modify_file_test "cursor15" somelines cursor_left (0, 0, 0);
  modify_file_test "cursor16" (move_cursor somelines 5) cursor_left (4, 0, 4);
  modify_file_test "cursor17" (move_cursor somelines 6) cursor_left (5, 0, 5);
  modify_file_test "cursor18" (move_cursor somelines 13) cursor_left (12, 2, 0);
  modify_file_test "cursor19" (move_cursor somelines 16) cursor_right (16, 3, 3);
  modify_file_test "cursor20" (move_cursor somelines 14) cursor_right (15, 3, 2);
  modify_file_test "cursor21" (move_cursor somelines 5) cursor_right (6, 1, 0);
  modify_file_test "cursor22" somelines cursor_up (0, 0, 0);
  modify_file_test "cursor23" (move_cursor somelines 3) cursor_up (0, 0, 0);
  modify_file_test "cursor24" (move_cursor somelines 8) cursor_up (2, 0, 2);
  modify_file_test "cursor25" (move_cursor somelines 15) cursor_up (12, 2, 0);
  modify_file_test "cursor26" (move_cursor somelines 12) cursor_up (6, 1, 0);
  modify_file_test "cursor27" (move_cursor somelines 16) cursor_down (16, 3, 3);
  modify_file_test "cursor28" (move_cursor somelines 14) cursor_down (16, 3, 3);
  modify_file_test "cursor29" (move_cursor somelines 2) cursor_down (8, 1, 2);
  modify_file_test "cursor30" (move_cursor somelines 11) cursor_down (12, 2, 0);

  (* scrolling tests *)
  "scroll0" >:: (fun _ -> assert_equal 3
    (scroll_to somelines 3 |> get_scroll_line));
  "scroll1" >:: (fun _ -> assert_equal 0
    (scroll_to somelines (-1) |> get_scroll_line));
  "scroll2" >:: (fun _ -> assert_equal 3
    (scroll_to somelines 4 |> get_scroll_line));
  "scroll3" >:: (fun _ -> assert_equal 0
    (scroll_to somelines 0 |> get_scroll_line));

  (* substring *)
  "substr0" >:: (fun _ -> assert_equal "llo\nwo"
    (get_text somelines 2 8));
  "substr1" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    (get_text somelines 0 17) ~printer: (fun s -> "\"" ^ s ^ "\""));
  "substr2" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    (get_text somelines (-1) 18));
  "substr3" >:: (fun _ -> assert_equal "hello"
    (get_text somelines 5 0));

  (* selecting text *)
  "select0" >:: (fun _ -> assert_equal None
    (get_selected_range somelines));
  "select1" >:: (fun _ -> assert_equal (Some (3,9))
    (select_text somelines 3 9 |> get_selected_range));
  "select2" >:: (fun _ -> assert_equal (Some (0,17))
    (select_text somelines (-1) 18 |> get_selected_range));
  "select3" >:: (fun _ -> assert_equal (Some (0,5))
    (select_text somelines 5 0 |> get_selected_range));
  "select4" >:: (fun _ -> assert_equal None
    (select_text somelines 5 0 |> unselect_text |> get_selected_range));

  (* inserting text *)
  insert_test "insert0" somelines "oh, " 0
    "oh, hello\nworld\n\n!!!\n" [10; 6; 1; 4];
  insert_test "insert1" somelines "n" 2
    "henllo\nworld\n\n!!!\n" [7; 6; 1; 4];
  insert_test "insert2" somelines "o\nk" 3
    "helo\nklo\nworld\n\n!!!\n" [5; 4; 6; 1; 4];
  insert_test "insert3" somelines "yo" (-1)
    "yohello\nworld\n\n!!!\n" [8; 6; 1; 4];
  insert_test "insert4" somelines "yo" 17
    "hello\nworld\n\n!!!\nyo\n" [6; 6; 1; 4; 3];
  insert_test "insert5" somelines "ok\n" 20
    "hello\nworld\n\n!!!\nok\n" [6; 6; 1; 4; 3];

  (* inserting char *)
  ins_ch_test "insch0" somelines 'a' 0
    "ahello\nworld\n\n!!!\n" [7; 6; 1; 4];
  ins_ch_test "insch1" somelines 'o' 17
    "hello\nworld\n\n!!!o\n" [6; 6; 1; 5];
  ins_ch_test "insch2" (delete_text somelines 16 17) 'o' 16
    "hello\nworld\n\n!!!o\n" [6; 6; 1; 5];
  ins_ch_test "insch3" somelines ' ' 8
    "hello\nwo rld\n\n!!!\n" [6; 7; 1; 4];
  ins_ch_test "insch4" somelines '\n' 8
    "hello\nwo\nrld\n\n!!!\n" [6; 3; 4; 1; 4];
  ins_ch_test "insch5" somelines '\n' 2
    "he\nllo\nworld\n\n!!!\n" [3; 4; 6; 1; 4];
  ins_ch_test "insch6" somelines '\n' 16
    "hello\nworld\n\n!!!\n\n" [6; 6; 1; 4; 1];
  ins_ch_test "insch7" somelines '\n' 14
    "hello\nworld\n\n!\n!!\n" [6; 6; 1; 2; 3];

  (* deleting text *)
  delete_test "delete0" somelines 0 3
    "lo\nworld\n\n!!!\n" [3; 6; 1; 4];
  delete_test "delete1" somelines 4 7
    "hellorld\n\n!!!\n" [9; 1; 4];
  delete_test "delete2" somelines 12 6
    "hello\n\n!!!\n" [6; 1; 4];
  delete_test "delete3" somelines 0 17 "\n" [1];
  delete_test "delete4" somelines (-1) 18 "\n" [1];
  delete_test "delete5" somelines 0 16 "\n" [1];
  delete_test "delete6" somelines 15 17 
    "hello\nworld\n\n!!\n" [6; 6; 1; 3];

  (* saving a file *)
  "save" >:: (fun _ -> assert_equal "abcde\n" (
    somelines
    |> fun f -> delete_text f 0 17
    |> fun f -> insert_text f "abcde" 0
    |> fun f -> save_file f "testtxts/temp.txt";
    open_file "testtxts/temp.txt" |> get_all_text
  ) ~printer: (fun s -> s));

  (* tests for setting and getting the search term of a file *)
  "find0" >:: (fun _ -> assert_equal (Some "hello")
    (get_search_term (find somelines ("hello"))));
  "find1" >:: (fun _ -> assert_equal (Some " ")
    (get_search_term (find somelines " ")));
  "find2" >:: (fun _ -> assert_equal None
    (get_search_term (find somelines "")));
  "find3" >:: (fun _ -> assert_equal None
    (get_search_term somelines));


  (* tests for selecting the search term of a file *)
  "sel_search0" >:: (fun _ -> assert_equal (Some (0,1))
    ((find somelines "h") |> select_search_term |> get_selected_range));
  (* loop back to start at end of file *)
  "sel_search1" >:: (fun _ -> assert_equal (Some (0,1))
    ((find somelines "h") |> select_search_term |> select_search_term |> get_selected_range));
  (* term not found *)
  "sel_search2" >:: (fun _ -> assert_equal None
    ((find somelines "Hello") |> select_search_term |> get_selected_range));
  (* first location *)
  "sel_search3" >:: (fun _ -> assert_equal (Some (2,3))
    ((find somelines "l") |> select_search_term |> get_selected_range));
  (* second location *)
  "sel_search4" >:: (fun _ -> assert_equal (Some (3,4))
    ((find somelines "l") |> select_search_term |> select_search_term |> get_selected_range));
  (* third location *)
  "sel_search5" >:: (fun _ -> assert_equal (Some (9,10))
    ((find somelines "l") |> select_search_term |> select_search_term |> select_search_term |> get_selected_range));

  (* tests for removing the search term of a file *)
  "rem_find0" >:: (fun _ -> assert_equal None
    ((find somelines "hello") |> remove_search_term |> get_search_term));
  "rem_find1" >:: (fun _ -> assert_equal None
    ((find somelines " ") |> remove_search_term |> get_search_term));

  (* tests for setting, getting, and removing the replace term of a file *)
  "rep0" >:: (fun _ -> assert_equal None
    (somelines |> get_replace_term));
  "rep1" >:: (fun _ -> assert_equal (Some "H")
    ((set_replace_term somelines "H") |> get_replace_term));
  "rep2" >:: (fun _ -> assert_equal None
    ((set_replace_term somelines "H") |> remove_replace_term |> get_replace_term));
  "rep3" >:: (fun _ -> assert_equal (Some "")
    ((set_replace_term somelines "") |> get_replace_term));

  (* tests for replacing the next search term *)
  "rep_next0" >:: (fun _ -> assert_equal "Hello\nworld\n\n!!!\n"
    ((set_replace_term (find somelines "h") "H") |> replace_next |> get_all_text));
  "rep_next5" >:: (fun _ -> assert_equal (Some (0,1))
    ((set_replace_term (find somelines "h") "H") |> replace_next |> get_selected_range));
  "rep_next1" >:: (fun _ -> assert_equal "Hello\nHorld\n\n!!!\n"
    ((find ((set_replace_term (find somelines "h") "H") |> replace_next) "w") |> replace_next |> get_all_text));
  "rep_next2" >:: (fun _ -> assert_equal "helloworld!!!\n"
    ((set_replace_term (find somelines "\n") "") |> replace_next |> replace_next |> replace_next |> replace_next |> get_all_text));
  "rep_next3" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((set_replace_term (find somelines "") "H") |> replace_next |> get_all_text));
  "rep_next4" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    ((find somelines "h") |> replace_next |> get_all_text));

  (* saving a file *)
  "save" >:: (fun _ -> assert_equal "abcde\n" (
    somelines
    |> fun f -> delete_text f 0 17
    |> fun f -> insert_text f "abcde" 0
    |> fun f -> save_file f "testtxts/temp.txt";
    open_file "testtxts/temp.txt" |> get_all_text
  ) ~printer: (fun s -> s));
]
