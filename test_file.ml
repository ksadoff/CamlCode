open OUnit2
open File

(* Test cases for File module. *)
let tests = [
  "read_file" >:: (fun _ -> assert_equal "test file\n"
    (File.open_file "testtxts/easy.txt" |> File.get_all_text)
  );
  "read_somelines" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n" 
    (File.open_file "testtxts/somelines.txt" |> File.get_all_text)
    ~printer: (fun x -> x)
  );
]