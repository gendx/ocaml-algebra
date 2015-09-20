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
open Ring
open TestGroup

module TestRing (R : Ring) = struct

  (* Args    : three non-zero elements of ring R *)
  (* Returns : a test suite for ring properties of R *)
  let test_me (x : R.t) (y : R.t) (z : R.t) : OUnit.test =
    ("Ring(" ^ (R.to_string x) ^ ", " ^ (R.to_string y) ^ ", " ^ (R.to_string z) ^ ")") >:::
    [
      "is_one" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (R.is_one (R.zero ()))
            false) ;
        "(2)" >:: (fun _ -> assert_equal
            (R.is_one (R.one ()))
            true) ;
      ] ;

      "of_int" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (R.of_int 0)
            (R.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (R.of_int 1)
            (R.one ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (R.of_int 6)
            (R.mul_int 6 (R.one ()))) ;
      ] ;
      "mul" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (R.mul x (R.one ()))
            x) ;
        "(2)" >:: (fun _ -> assert_equal
            (R.mul (R.one ()) x)
            x) ;
        "(3)" >:: (fun _ -> assert_equal
            (R.mul x (R.mul y z))
            (R.mul (R.mul x y) z)) ;
        "(4)" >:: (fun _ -> assert_equal
            (R.mul x (R.zero ()))
            (R.zero ())) ;
        "(5)" >:: (fun _ -> assert_equal
            (R.mul (R.zero ()) x)
            (R.zero ())) ;
        "(6)" >:: (fun _ -> assert_equal
            (R.mul x (R.opposite y))
            (R.opposite (R.mul x y))) ;
        "(7)" >:: (fun _ -> assert_equal
            (R.mul x (R.add y z))
            (R.add (R.mul x y) (R.mul x z))) ;
        "(8)" >:: (fun _ -> assert_equal
            (R.mul x (R.sub y z))
            (R.sub (R.mul x y) (R.mul x z))) ;
      ] ;
    ]

  module TestGroupImpl = TestGroup(R)

  let test (x : R.t) (y : R.t) (z : R.t) : OUnit.test list =
    (test_me x y z)::(TestGroupImpl.test x y z)

end


module TestCommutativeRing (R : Ring) = struct

  (* Args    : three non-zero elements of commutative ring R *)
  (* Returns : a test suite for commutative ring properties of R *)
  let test_me (x : R.t) (y : R.t) (z : R.t) : OUnit.test =
    ("CommutativeRing(" ^ (R.to_string x) ^ ", " ^ (R.to_string y) ^ ", " ^ (R.to_string z) ^ ")") >:::
    [
      "mul" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (R.mul x y)
            (R.mul y x)) ;
      ] ;
    ]

  module TestRingImpl = TestRing(R)

  let test (x : R.t) (y : R.t) (z : R.t) : OUnit.test list =
    (test_me x y z)::(TestRingImpl.test x y z)

end

