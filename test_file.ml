open OUnit2
open File

(* Test cases for File module. *)
let tests = [
  "read_file" >:: (fun _ -> assert_equal "test file"
    (File.open_file "testtxts/easy.txt" |> File.get_all_text)
  )
]