open OUnit2

let suite = "CamlCode test suite" >::: Test_file.tests

let _ = run_test_tt_main suite