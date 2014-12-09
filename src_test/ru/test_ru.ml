open OUnit2

let test_Цвет () =
  assert_equal "Красный" (Цвет.show_цвет Цвет.Красный)

let suite = "ru" >: [
    "test_Цвет" >:: test_Цвет
  ]
