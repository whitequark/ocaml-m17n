open OUnit2

let suite = [
    Test_ru.suite;
    Test_ja.suite;
  ]

let _ = run_test_tt_main suite
