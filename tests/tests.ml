open OUnit2

let tests = [
  "PTArray" >::: PTArrayTests.tests;
]

let () =
  run_test_tt_main ("Tests" >::: tests)

