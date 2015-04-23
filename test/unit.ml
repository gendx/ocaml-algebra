open OUnit

let tests =
  "OCaml-algebra">:::
  [
    TestCommon.tests;
    TestFractional.tests;
    TestMisc.tests;
  ]

let _ =
  run_test_tt_main tests

