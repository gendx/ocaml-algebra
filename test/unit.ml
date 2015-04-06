open OUnit

module Q = Common.Q

let suite_Q =
  "suite Q">:::
  [
    "test_make_1">::(fun test_ctxt -> assert_equal (Q.make 3 4) (Q.make (-3) (-4)));
    "test_make_2">::(fun test_ctxt -> assert_equal (Q.make (-7) 2) (Q.make 7 (-2)));
    "test_make_3">::(fun test_ctxt -> assert_equal (Q.make 2 (-6)) (Q.make (-3) 9));
    "test_add_1">::(fun test_ctxt -> assert_equal (Q.add (Q.make 2 3) (Q.make 3 4)) (Q.make 17 12));
    "test_sub_1">::(fun test_ctxt -> assert_equal (Q.sub (Q.make 2 3) (Q.make 3 7)) (Q.make 5 21));
    "test_mul_1">::(fun test_ctxt -> assert_equal (Q.mul (Q.make 5 7) (Q.make 3 2)) (Q.make 15 14));
    "test_mul_2">::(fun test_ctxt -> assert_equal (Q.mul (Q.make (-2) 3) (Q.make (-3) (-5))) (Q.make (-2) 5));
    "test_div_1">::(fun test_ctxt -> assert_equal (Q.div (Q.make 7 9) (Q.make 6 4)) (Q.make 28 54))
  ]

let _ =
  run_test_tt_main suite_Q

