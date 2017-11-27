open OUnit2
open File

let int_list_printer l = List.fold_left 
  (fun acc x -> acc ^ (string_of_int x) ^ "; ") "" l
  |> fun s -> "[" ^ s ^ "]"

let somelines = File.open_file "testtxts/somelines.txt"

let somelines_moved = move_cursor somelines 12

(* [move_cursor_test f exp] returns a test case where
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
]