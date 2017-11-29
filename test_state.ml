open OUnit2
open State

let slstate = empty_state |> fun st -> open_file st "testtxts/somelines.txt"

let basic_clipboard = Rope.make 10 'a'

let basic_state = empty_state |> fun sr -> open_file st "testtxts/clipboardtest.txt"
let basic_state_file = File.set_selected_range (get_current_file basic_state) (0, 9)
let basic_state' = set_current_file basic_state basic_state_file
let basic_state_paste_file = File.move_cursor basic_state_file 10
let basic_state_paste = set_current_file basic_state' basic_state_paste_file

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


  (*clipboard*)
  "clipboard empty" >:: (fun _ -> assert_equal Rope.empty (new_clipboard));
  "clipboard copy" >:: (fun _ -> assert_equal basic_clipboard (copy basic_state'));
    "clipboard paste" >:: (fun _ -> assert_equal concat2 basic_clipboard basic_clipboard
                              (paste basic_state_paste))

]
