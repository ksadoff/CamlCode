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
]