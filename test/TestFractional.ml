open OUnit
open Common

let testQ =
  "Q" >:::
  [
    "make" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Q.make 3 4) (Q.make (-3) (-4))) ;
      "(2)" >:: (fun test_ctxt -> assert_equal (Q.make (-7) 2) (Q.make 7 (-2))) ;
      "(3)" >:: (fun test_ctxt -> assert_equal (Q.make 2 (-6)) (Q.make (-3) 9)) ;
    ] ;
    "add" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Q.add (Q.make 2 3) (Q.make 3 4)) (Q.make 17 12)) ;
      "(2)" >:: (fun test_ctxt -> assert_equal (Q.add (Q.make 2 3) (Q.make 8 6)) (Q.make 2 1)) ;
    ] ;
    "sub" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Q.sub (Q.make 2 3) (Q.make 3 7)) (Q.make 5 21)) ;
      "(2)" >:: (fun test_ctxt -> assert_equal (Q.sub (Q.make 2 3) (Q.make 3 (-4))) (Q.make 17 12)) ;
    ] ;
    "mul" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Q.mul (Q.make 5 7) (Q.make 3 2)) (Q.make 15 14)) ;
      "(2)" >:: (fun test_ctxt -> assert_equal (Q.mul (Q.make (-2) 3) (Q.make (-3) (-5))) (Q.make (-2) 5)) ;
    ] ;
    "div" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Q.div (Q.make 7 9) (Q.make 6 4)) (Q.make 28 54)) ;
    ] ;
  ]

let tests =
  "Fractional">:::
  [
    testQ
  ]

