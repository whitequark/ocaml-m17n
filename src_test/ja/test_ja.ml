open OUnit2

type 色 =
| @赤
| @黄色
| @緑色

let show_色 =
  function
  | @赤 -> "赤"
  | @黄色 -> "黄色"
  | @緑色 -> "緑色"

let test_色 ctxt =
  assert_equal "赤" (show_色 @赤)

let suite = "ja" >::: [
    "test_赤" >:: test_色
  ]
