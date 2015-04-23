open OUnit
open Common

let testZ =
  "Z" >:::
  [
    "zero" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Z.zero ()) (Z.of_int 0)) ;
    ] ;
    "one" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Z.one ()) (Z.of_int 1)) ;
    ] ;
    "is_zero" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Z.is_zero (Z.zero ())) true) ;
      "(2)" >:: (fun test_ctxt -> assert_equal (Z.is_zero (Z.one ())) false) ;
      "(3)" >:: (fun test_ctxt -> assert_equal (Z.is_zero (Z.of_int 3)) false) ;
    ] ;
    "is_one" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Z.is_one (Z.zero ())) false) ;
      "(2)" >:: (fun test_ctxt -> assert_equal (Z.is_one (Z.one ())) true) ;
      "(3)" >:: (fun test_ctxt -> assert_equal (Z.is_one (Z.of_int 6)) false) ;
    ] ;
    "opposite" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Z.opposite (Z.of_int 5)) (Z.of_int (-5))) ;
      "(2)" >:: (fun test_ctxt -> assert_equal (Z.opposite (Z.of_int (-3))) (Z.of_int 3)) ;
      "(3)" >:: (fun test_ctxt -> assert_equal (Z.opposite (Z.opposite (Z.of_int 7))) (Z.of_int 7)) ;
    ] ;
    "of_int" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Z.of_int 9) 9) ;
    ] ;
    "mul_int" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Z.mul_int 4 (Z.of_int 2)) (Z.of_int 8)) ;
    ] ;
  ]

let tests =
  "Common">:::
  [
    testZ
  ]

