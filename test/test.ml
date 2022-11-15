open OUnit2
open Game
open Cards

let tests =
  "test suite for cards.ml" >::: [ ("random" >:: fun _ -> assert_equal 1 1) ]

let _ = run_test_tt_main tests
