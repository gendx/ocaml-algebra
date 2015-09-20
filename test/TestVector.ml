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
open Errors
open Vector
open TestVectorSpace

module VectQ3 = MakeVector(Q)(struct let x = 3 end)
module TestVectorSpaceVQ3 = TestVectorSpace(Q)(VectQ3)

let q_make_of_pair (x, y) = Q.make x y

let vect_of_ints = VectQ3.make_of Q.of_int

let testVectorQ3 =
  "VectorQ3" >:::
  (TestVectorSpaceVQ3.test (vect_of_ints [|1 ; 2 ; 3|]) (vect_of_ints [|7 ; 4 ; 8|]) (vect_of_ints [|6 ; 4 ; 3|]) (Q.of_int 11) (Q.of_int 14))
  @ [
    "to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (VectQ3.to_string (VectQ3.zero ()))
          "[0 0 0]") ;
      "(2)" >:: (fun _ -> assert_equal
          (VectQ3.to_string (vect_of_ints [|1 ; 2 ; 3|]))
          "[1 2 3]") ;
      "(3)" >:: (fun _ -> assert_equal
          (VectQ3.to_string (VectQ3.make_of q_make_of_pair [|4, 2 ; 3, 7 ; 9, (-6)|]))
          "[2 3/7 -3/2]") ;
    ] ;
    "make" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (VectQ3.make [|Q.of_int 1 ; Q.of_int 2 ; Q.of_int 3|])
          [|Q.of_int 1 ; Q.of_int 2 ; Q.of_int 3|]) ;
      "(2)" >:: (fun _ -> assert_raises
          Errors.Invalid_dimension
          (fun () -> VectQ3.make [|Q.of_int 1 ; Q.of_int 2|])) ;
    ] ;
    "make_of" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (vect_of_ints [|4 ; 5 ; 6|])
          [|Q.of_int 4 ; Q.of_int 5 ; Q.of_int 6|]) ;
      "(2)" >:: (fun _ -> assert_raises
          Errors.Invalid_dimension
          (fun () -> vect_of_ints [|1 ; 2|])) ;
    ] ;

    "zero" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (VectQ3.zero ())
          [|Q.zero () ; Q.zero () ; Q.zero ()|]) ;
    ] ;
    "is_zero" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (VectQ3.is_zero (vect_of_ints [|0 ; 0 ; 0|]))
          true) ;
      "(2)" >:: (fun _ -> assert_equal
          (VectQ3.is_zero (vect_of_ints [|4 ; 5 ; 6|]))
          false) ;
      "(3)" >:: (fun _ -> assert_equal
          (VectQ3.is_zero (vect_of_ints [|0 ; 0 ; 1|]))
          false) ;
    ] ;

    "opposite" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (VectQ3.opposite (vect_of_ints [|1 ; -2 ; 3|]))
          (vect_of_ints [|-1 ; 2 ; -3|])) ;
    ] ;
    "mul_int" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (VectQ3.mul_int 4 (vect_of_ints [|1 ; -2 ; 3|]))
          (vect_of_ints [|4 ; -8 ; 12|])) ;
    ] ;
    "mul_field" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (VectQ3.mul_field (Q.make_of_int 7 4) (vect_of_ints [|1 ; -2 ; 3|]))
          (VectQ3.make_of q_make_of_pair [|7, 4 ; -7, 2 ; 21, 4|])) ;
    ] ;

    "add" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (VectQ3.add (vect_of_ints [|1 ; 2 ; 3|]) (vect_of_ints [|2 ; 3 ; 5|]))
          (vect_of_ints [|3 ; 5 ; 8|])) ;
    ] ;
    "sub" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (VectQ3.sub (vect_of_ints [|1 ; 2 ; 3|]) (vect_of_ints [|2 ; 3 ; 5|]))
          (vect_of_ints [|-1 ; -1 ; -2|])) ;
    ] ;

    "dot" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (VectQ3.dot (vect_of_ints [|1 ; 2 ; 3|]) (VectQ3.zero ()))
          (Q.zero ())) ;
      "(2)" >:: (fun _ -> assert_equal
          (VectQ3.dot (vect_of_ints [|1 ; 0 ; 1|]) (vect_of_ints [|0 ; 1 ; 0|]))
          (Q.zero ())) ;
      "(3)" >:: (fun _ -> assert_equal
          (VectQ3.dot (vect_of_ints [|1 ; 2 ; 3|]) (vect_of_ints [|2 ; 3 ; 5|]))
          (Q.of_int 23)) ;
    ] ;
  ]

let tests =
  "Vector" >:::
  [
    testVectorQ3 ;
  ]

