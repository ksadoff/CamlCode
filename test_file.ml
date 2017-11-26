open OUnit2
open File

let int_list_printer l = List.fold_left 
  (fun acc x -> acc ^ (string_of_int x) ^ "; ") "" l
  |> fun s -> "[" ^ s ^ "]"

let somelines = File.open_file "testtxts/somelines.txt"

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
    ~printer: int_list_printer)
]