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
open VectorSpace
open TestGroup

module TestVectorSpace (R : Ring) (V : VectorSpace with type ft = R.t) = struct

  (* Args    : three non-zero elements of vector-space V
               two non-zero elements of the underlying field of V *)
  (* Returns : a test suite for vector-space properties of V *)
  let test_me (x : V.t) (y : V.t) (z : V.t) (f : R.t) (g : R.t) : OUnit.test =
    ("VectorSpace(" ^ (V.to_string x) ^ ", " ^ (V.to_string y) ^ ", " ^ (V.to_string z) ^ ", " ^ (R.to_string f) ^ ", " ^ (R.to_string g) ^ ")") >:::
    [
      "mul_field" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (V.mul_field (R.zero ()) x)
            (V.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (V.mul_field f (V.zero ()))
            (V.zero ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (V.mul_field f (V.mul_field g x))
            (V.mul_field (R.mul f g) x)) ;
        "(4)" >:: (fun _ -> assert_equal
            (V.mul_field f (V.opposite x))
            (V.opposite (V.mul_field f x))) ;
        "(5)" >:: (fun _ -> assert_equal
            (V.mul_field (R.opposite f) x)
            (V.opposite (V.mul_field f x))) ;
        "(6)" >:: (fun _ -> assert_equal
            (V.mul_field f (V.add x y))
            (V.add (V.mul_field f x) (V.mul_field f y))) ;
        "(7)" >:: (fun _ -> assert_equal
            (V.mul_field f (V.sub x y))
            (V.sub (V.mul_field f x) (V.mul_field f y))) ;
      ] ;
    ]

  module TestGroupImpl = TestGroup(V)

  let test (x : V.t) (y : V.t) (z : V.t) (f : R.t) (g : R.t) : OUnit.test list =
    (test_me x y z f g)::(TestGroupImpl.test x y z)

end

