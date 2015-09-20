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
open Errors
open Ring
open Field
open Vector
open Matrix
open TestAlgebra

module TestSquaredMatrix (R : Ring) (M : SquaredMatrix with type ft = R.t) = struct

  module V = M.Vector

  (* Args    : three non-zero elements of matrix-space M
               two non-zero elements of vector-space V
               two non-zero elements of the underlying ring of M *)
  (* Returns : a test suite for matrix-space properties of M *)
  let test_me (x : M.t) (y : M.t) (z : M.t) (v : V.t) (w : V.t) (f : R.t) (g : R.t) : OUnit.test =
    ("SquaredMatrix(" ^ (M.to_string x) ^ ", " ^ (M.to_string y) ^ ", " ^ (M.to_string z) ^ ", " ^ (V.to_string v) ^ ", " ^ (V.to_string w) ^ ", " ^ (R.to_string f) ^ ", " ^ (R.to_string g) ^ ")") >:::
    [
      "transpose" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (M.transpose (M.zero ()))
            (M.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (M.transpose (M.one ()))
            (M.one ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (M.transpose (M.transpose x))
            x) ;
        "(4)" >:: (fun _ -> assert_equal
            (M.transpose (M.opposite x))
            (M.opposite (M.transpose x))) ;
        "(5)" >:: (fun _ -> assert_equal
            (M.transpose (M.mul_field f x))
            (M.mul_field f (M.transpose x))) ;
        "(6)" >:: (fun _ -> assert_equal
            (M.transpose (M.add x y))
            (M.add (M.transpose x) (M.transpose y))) ;
        "(7)" >:: (fun _ -> assert_equal
            (M.transpose (M.sub x y))
            (M.sub (M.transpose x) (M.transpose y))) ;
        "(8)" >:: (fun _ -> assert_equal
            (M.transpose (M.mul x y))
            (M.mul (M.transpose y) (M.transpose x))) ;
      ] ;
      "trace" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (M.trace (M.zero ()))
            (R.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (M.trace (M.opposite x))
            (R.opposite (M.trace x))) ;
        "(3)" >:: (fun _ -> assert_equal
            (M.trace (M.mul_field f x))
            (R.mul f (M.trace x))) ;
        "(4)" >:: (fun _ -> assert_equal
            (M.trace (M.add x y))
            (R.add (M.trace x) (M.trace y))) ;
        "(5)" >:: (fun _ -> assert_equal
            (M.trace (M.sub x y))
            (R.sub (M.trace x) (M.trace y))) ;
        "(6)" >:: (fun _ -> assert_equal
            (M.trace (M.mul x y))
            (M.trace (M.mul y x))) ;
        "(7)" >:: (fun _ -> assert_equal
            (M.trace (M.transpose x))
            (M.trace x)) ;
      ] ;
      "mul_vect" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (M.mul_vect (M.zero ()) v)
            (V.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (M.mul_vect (M.one ()) v)
            v) ;
        "(3)" >:: (fun _ -> assert_equal
            (M.mul_vect (M.opposite x) v)
            (V.opposite (M.mul_vect x v))) ;
        "(4)" >:: (fun _ -> assert_equal
            (M.mul_vect (M.add x y) v)
            (V.add (M.mul_vect x v) (M.mul_vect y v))) ;
        "(5)" >:: (fun _ -> assert_equal
            (M.mul_vect (M.sub x y) v)
            (V.sub (M.mul_vect x v) (M.mul_vect y v))) ;
        "(6)" >:: (fun _ -> assert_equal
            (M.mul_vect (M.mul x y) v)
            (M.mul_vect x (M.mul_vect y v))) ;
        "(7)" >:: (fun _ -> assert_equal
            (M.mul_vect x (V.zero ()))
            (V.zero ())) ;
        "(8)" >:: (fun _ -> assert_equal
            (M.mul_vect x (V.opposite v))
            (V.opposite (M.mul_vect x v))) ;
        "(9)" >:: (fun _ -> assert_equal
            (M.mul_vect x (V.add v w))
            (V.add (M.mul_vect x v) (M.mul_vect x w))) ;
        "(10)" >:: (fun _ -> assert_equal
            (M.mul_vect x (V.sub v w))
            (V.sub (M.mul_vect x v) (M.mul_vect x w))) ;
      ] ;
    ]

  module TestAlgebraImpl = TestAlgebra(R)(M)

  let test (x : M.t) (y : M.t) (z : M.t) (v : V.t) (w : V.t) (f : R.t) (g : R.t) : OUnit.test list =
    (test_me x y z v w f g)::(TestAlgebraImpl.test x y z f g)

end


module TestSquaredFieldMatrix (F : Field) (M : SquaredFieldMatrix with type ft = F.t) = struct

  module V = M.Vector

  (* Args    : three inversible elements of matrix-space M
               two non-zero elements of vector-space V
               two non-zero elements of the underlying ring of M *)
  (* Returns : a test suite for matrix-space properties of M *)
  let test_me (x : M.t) (y : M.t) (z : M.t) (v : V.t) (w : V.t) (f : F.t) (g : F.t) : OUnit.test =
    ("SquaredFieldMatrix(" ^ (M.to_string x) ^ ", " ^ (M.to_string y) ^ ", " ^ (M.to_string z) ^ ", " ^ (V.to_string v) ^ ", " ^ (V.to_string w) ^ ", " ^ (F.to_string f) ^ ", " ^ (F.to_string g) ^ ")") >:::

    [
      "determinant" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (M.determinant (M.zero ()))
            (F.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (M.determinant (M.transpose x))
            (M.determinant x)) ;
        "(3)" >:: (fun _ -> assert_equal
            (M.determinant (M.mul x y))
            (F.mul (M.determinant x) (M.determinant y))) ;
        "(4)" >:: (fun _ -> assert_equal
            (M.determinant (M.inverse x))
            (F.inverse (M.determinant x))) ;
      ] ;
      "inverse" >:::
      [
        "(1)" >:: (fun _ -> assert_raises
            Errors.Not_inversible
            (fun () -> M.inverse (M.zero ()))) ;
        "(2)" >:: (fun _ -> assert_equal
            (M.inverse (M.one ()))
            (M.one ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (M.mul x (M.inverse x))
            (M.one ())) ;
        "(4)" >:: (fun _ -> assert_equal
            (M.mul (M.inverse x) x)
            (M.one ())) ;
        "(5)" >:: (fun _ -> assert_equal
            (M.inverse (M.mul_field f x))
            (M.mul_field (F.inverse f) (M.inverse x))) ;
        "(6)" >:: (fun _ -> assert_equal
            (M.inverse (M.mul x y))
            (M.mul (M.inverse y) (M.inverse x))) ;
        "(7)" >:: (fun _ -> assert_equal
            (M.inverse (M.transpose x))
            (M.transpose (M.inverse x))) ;
      ] ;
    ]

  module TestSquaredMatrixImpl = TestSquaredMatrix(F)(M)

  let test (x : M.t) (y : M.t) (z : M.t) (v : V.t) (w : V.t) (f : F.t) (g : F.t) : OUnit.test list =
    (test_me x y z v w f g)::(TestSquaredMatrixImpl.test x y z v w f g)

end

