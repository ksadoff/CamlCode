open OUnit2

let suite = "CamlCode test suite" >::: 
  Test_file.tests @ Test_state.tests @ Test_color.tests

let _ = run_test_tt_main suite
