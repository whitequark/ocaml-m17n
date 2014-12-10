type 色 =
| @赤
| @黄色
| @緑色
[@@deriving show]

let test_色 () =
  assert_equal "赤" (show_色 @赤)

let suite = "ja" >: [
    "test_赤" >:: test_色
  ]
