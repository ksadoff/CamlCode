open OUnit2
open File

let int_list_printer l = List.fold_left 
  (fun acc x -> acc ^ (string_of_int x) ^ "; ") "" l
  |> fun s -> "[" ^ s ^ "]"

let somelines = File.open_file "testtxts/somelines.txt"

(* [move_cursor_test f exp] returns a test case where
 * the cursor in file [f] is moved to index [l]. [exp] is
 * the expected (index, line num, column) tuple. [test_name] is
 * name of the test, used by OUnit. *)
let move_cursor_test test_name orig_f l exp = 
  let f = orig_f |> fun f' -> move_cursor f' l in 
  test_name >:: (fun _ -> assert_equal exp 
    (File.get_cursor_location f, 
    File.get_cursor_line_num f, 
    File.get_cursor_column f)
    ~printer: (fun (i,l,c) -> "(" ^ (string_of_int i) ^ ", " ^ 
      (string_of_int l) ^ ", " ^ 
      (string_of_int c) ^ ")")
  )

(* Test cases for File module. *)
let tests = [
  "read_file" >:: (fun _ -> assert_equal "test file\n"
    (File.open_file "testtxts/easy.txt" |> File.get_all_text)
  );
  "read_somelines" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n" 
    (File.open_file "testtxts/somelines.txt" |> File.get_all_text)
    ~printer: (fun x -> x)
  );
  "line_lengths" >:: (fun _ -> assert_equal [6; 6; 1; 4] 
    (somelines |> File.get_line_lengths)
    ~printer: int_list_printer);
  move_cursor_test "cursor0" somelines 1 (1, 0, 1);
]