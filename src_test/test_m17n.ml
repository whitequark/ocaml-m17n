open OUnit2

let suite = "Test m17n" >::: [
    Test_ru.suite;
    Test_ja.suite;
    Test_zh_HK.suite;
  ]

let _ = run_test_tt_main suite
