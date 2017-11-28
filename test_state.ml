open OUnit2
open State

let slstate = empty_state |> fun st -> open_file st "testtxts/somelines.txt"

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
]