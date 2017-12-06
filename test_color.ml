open OUnit2
open Color



let tests = [
  (* open and read file *)
  "compare less" >:: (fun _ -> assert_equal (-1) 
  (Color.compare' (0, make_color(0, 0, 0)) (255, make_color(255, 255, 255))));

  "compare equal" >:: (fun _ -> assert_equal (0) 
  (Color.compare' (100, make_color(255, 255, 255)) (100, make_color(100, 100, 100))));
  
  "compare more" >:: (fun _ -> assert_equal (1) 
  (Color.compare' (200, make_color(25, 253, 255)) (100, make_color(100, 100, 100))));

  (* NEED TO FIX FOLLOWING TESTS SOMEHOW: *)

  "add color replace" >:: (fun _ -> assert_equal 
    (make_cm([
      (0, make_color (100, 100, 100)); 
      (5, make_color(255, 255, 255))
      ])) 
    (Color.add_color empty_cm (0, 5, make_color (100, 100, 100))));

  "add color again" >:: (fun _ -> assert_equal 
    (make_cm[
      (0, make_color(255, 255, 255)); 
      (5, make_color(200, 200, 200)); 
      (10, make_color(255, 255, 255))
      ]) 
    (add_color empty_cm (5, 10, make_color(200, 200, 200))));

  "add color first" >:: (fun _ ->  assert_equal 
    (make_cm [
      (0, make_color(255, 255, 255)); 
      (3, make_color (100, 100, 100)); 
      (5, make_color (255, 255, 255))
      ]) 
    (add_color empty_cm (3, 5, make_color (100, 100, 100))));
]
