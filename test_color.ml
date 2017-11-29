open OUnit2
open Color



let tests = [
  (* open and read file *)
  "compare less" >:: (fun _ -> assert_equal (-1) (compare (0, (0, 0, 0)) (255, (255, 255, 255))));
  "compare equal" >:: (fun _ -> assert_equal (0) (compare (100, (255, 255, 255)) (100, (100, 100, 100))));
  "compare more" >:: (fun _ -> assert_equal (1) (compare (200, (25, 253, 255)) (100, (100, 100, 100))));

  (* NEED TO FIX FOLLOWING TESTS SOMEHOW: *)

  (* "add color replace" >:: (fun _ -> 
    assert_equal ([(0, (100, 100, 100)); (5, (255, 255, 255))]) 
    (add_color empty_cm (0, 5, (100, 100, 100))));
  "add color again" >:: (fun _ -> assert_equal 
    ([(0, (100, 100, 100)); (5, (200, 200, 200)); (10, (255, 255, 255))]) 
    (add_color empty_cm (5, 10, (200, 200, 200))));
  "add color first" >:: (fun _ -> 
    assert_equal ([(0, (255, 255, 255)); (3, (100, 100, 100)); (5, (255, 255, 255))]) 
    (add_color empty_cm (3, 5, (100, 100, 100)))); *)
]
