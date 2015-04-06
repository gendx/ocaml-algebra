open OUnit
open Misc

let fun1 x y = x > y
let fun2 x = true
let fun3 y = false

let comp x y =
  if x = y then
    0
  else if x < y then
    -1
  else
    1


let tests =
  "Misc" >:::
  [
    "map3" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Misc.map3 fun1 fun2 fun3 [1 ; 4 ; 3] [2 ; 3]) [false ; true ; true]) ;
      "(2)" >:: (fun test_ctxt -> assert_equal (Misc.map3 fun1 fun2 fun3 [7 ; 2] [5 ; 3 ; -10]) [true ; false ; false])
    ] ;
    "lexcompare" >:::
    [
      "(1)" >:: (fun test_ctxt -> assert_equal (Misc.lexcompare comp [1 ; 2 ; 3] [1 ; 2]) 1) ;
      "(2)" >:: (fun test_ctxt -> assert_equal (Misc.lexcompare comp [5 ; 2 ; 3] [5 ; 2 ; 3 ; 7]) (-1)) ;
      "(3)" >:: (fun test_ctxt -> assert_equal (Misc.lexcompare comp [1 ; 2 ; 3] [1 ; 3]) (-1))
    ]
  ]

