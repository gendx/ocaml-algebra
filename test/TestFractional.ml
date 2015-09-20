(*
    OCaml-algebra - an algebra system written in OCaml.
    Copyright (C) 2015  G. Endignoux

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see http://www.gnu.org/licenses/gpl-3.0.txt
*)

open OUnit
open Common
open TestField

module TestFieldQ = TestField(Q)

let testQ =
  "Q" >:::
  (TestFieldQ.test (Q.make 1 2) (Q.make 3 4) (Q.make 5 7))
  @ [
    "to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.to_string (Q.make 3 4))
          "3/4") ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.to_string (Q.make 3 (-7)))
          "-3/7") ;
      "(3)" >:: (fun _ -> assert_equal
          (Q.to_string (Q.of_int 17))
          "17") ;
      "(4)" >:: (fun _ -> assert_equal
          (Q.to_string (Q.zero ()))
          "0") ;
    ] ;
    
    "make" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.make 3 4)
          (Q.make (-3) (-4))) ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.make (-7) 2)
          (Q.make 7 (-2))) ;
      "(3)" >:: (fun _ -> assert_equal
          (Q.make 2 (-6))
          (Q.make (-3) 9)) ;
    ] ;
    "make_of_int" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.make_of_int 3 4)
          (Q.make 3 4)) ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.make_of_int 7 (-2))
          (Q.make 7 (-2))) ;
    ] ;
    
    "zero" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.zero ())
          (Q.make 0 1)) ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.zero ())
          (Q.make 0 7)) ;
    ] ;
    "one" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.one ())
          (Q.make 1 1)) ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.one ())
          (Q.make (-3) (-3))) ;
    ] ;
    "is_one" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.is_one (Q.make 3 4))
          false) ;
    ] ;
    
    "opposite" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.opposite (Q.make 2 3))
          (Q.make (-2) 3)) ;
    ] ;
    "inverse" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.inverse (Q.make 5 7))
          (Q.make 7 5)) ;
    ] ;
    "of_int" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.of_int 3)
          (Q.make 3 1)) ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.of_int 0)
          (Q.zero ())) ;
    ] ;
    "mul_int" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.mul_int 3 (Q.make 7 6))
          (Q.make 7 2)) ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.mul_int 3 (Q.make 7 5))
          (Q.make 21 5)) ;
    ] ;
    
    "add" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.add (Q.make 2 3) (Q.make 3 4))
          (Q.make 17 12)) ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.add (Q.make 2 3) (Q.make 8 6))
          (Q.make 2 1)) ;
    ] ;
    "sub" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.sub (Q.make 2 3) (Q.make 3 7))
          (Q.make 5 21)) ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.sub (Q.make 2 3) (Q.make 3 (-4)))
          (Q.make 17 12)) ;
    ] ;
    "mul" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.mul (Q.make 5 7) (Q.make 3 2))
          (Q.make 15 14)) ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.mul (Q.make (-2) 3) (Q.make (-3) (-5)))
          (Q.make (-2) 5)) ;
    ] ;
    "div" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Q.div (Q.make 7 9) (Q.make 6 4))
          (Q.make 28 54)) ;
      "(2)" >:: (fun _ -> assert_equal
          (Q.div (Q.make 3 4) (Q.make 5 6))
          (Q.mul (Q.make 3 4) (Q.make 6 5))) ;
    ] ;
  ]

let tests =
  "Fractional" >:::
  [
    testQ ;
  ]

