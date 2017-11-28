open OUnit2
open State

(* Test cases for File module. *)
let tests = [
  (* open and read file *)
  "read_somelines" >:: (fun _ -> assert_equal "hello\nworld\n\n!!!\n"
    (empty_state 
      |> fun st -> open_file st "testtxts/somelines.txt" 
      |> get_all_text
    )
    ~printer: (fun x -> x)
  );

  (* get_text *)
  "read_somelines" >:: (fun _ -> assert_equal "hello"
    (empty_state 
      |> fun st -> open_file st "testtxts/somelines.txt" 
      |> fun st -> get_text st 0 5
    )
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
]