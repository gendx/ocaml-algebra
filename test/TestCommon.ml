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

let testZ =
  "Z" >:::
  [
    "zero" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Z.zero ())
          (Z.of_int 0)) ;
    ] ;
    "one" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Z.one ())
          (Z.of_int 1)) ;
    ] ;
    "is_zero" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Z.is_zero (Z.zero ()))
          true) ;
      "(2)" >:: (fun _ -> assert_equal
          (Z.is_zero (Z.one ()))
          false) ;
      "(3)" >:: (fun _ -> assert_equal
          (Z.is_zero (Z.of_int 3))
          false) ;
    ] ;
    "is_one" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Z.is_one (Z.zero ()))
          false) ;
      "(2)" >:: (fun _ -> assert_equal
          (Z.is_one (Z.one ()))
          true) ;
      "(3)" >:: (fun _ -> assert_equal
          (Z.is_one (Z.of_int 6))
          false) ;
    ] ;
    "opposite" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Z.opposite (Z.of_int 5))
          (Z.of_int (-5))) ;
      "(2)" >:: (fun _ -> assert_equal
          (Z.opposite (Z.of_int (-3)))
          (Z.of_int 3)) ;
      "(3)" >:: (fun _ -> assert_equal
          (Z.opposite (Z.opposite (Z.of_int 7)))
          (Z.of_int 7)) ;
    ] ;
    "of_int" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Z.of_int 9)
          9) ;
    ] ;
    "mul_int" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (Z.mul_int 4 (Z.of_int 2))
          (Z.of_int 8)) ;
    ] ;
  ]

let tests =
  "Common">:::
  [
    testZ
  ]

