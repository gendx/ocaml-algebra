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
open Field
open Polynomial
open TestAlgebra

module TestPolynomialFunctions (R : Ring) (P : Polynomial with type ft = R.t) = struct

  let derivate : (P.t -> P.t) = P.derivate (P.one ()) (fun _ -> R.zero ())

  (* Args    : two non-zero elements of polynomial-space P *)
  (* Returns : a test suite for polynomial properties of P *)
  let test_me (x : P.t) (y : P.t) : OUnit.test =
    ("PolynomialFunctions(" ^ (P.to_string x) ^ ", " ^ (P.to_string y) ^ ")") >:::
    [
      "to_string_var" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (P.to_string_var "foo" (P.x ()))
            "(foo)") ;
        "(2)" >:: (fun _ -> assert_equal
            (P.to_string_var "bar" (P.zero ()))
            ("(" ^ (R.to_string (R.zero ())) ^ ")")) ;
      ] ;
      "make_monom" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (P.make_monom (R.zero ()) 0)
            (P.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (P.make_monom (R.zero ()) 3)
            (P.zero ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (P.make_monom (R.one ()) 0)
            (P.one ())) ;
        "(4)" >:: (fun _ -> assert_equal
            (P.make_monom (R.one ()) 1)
            (P.x ())) ;
      ] ;
      "derivate" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (derivate (P.zero ()))
            (P.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (derivate (P.one ()))
            (P.zero ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (derivate (P.x ()))
            (P.one ())) ;
        "(4)" >:: (fun _ -> assert_equal
            (derivate (P.add x y))
            (P.add (derivate x) (derivate y))) ;
        "(5)" >:: (fun _ -> assert_equal
            (derivate (P.sub x y))
            (P.sub (derivate x) (derivate y))) ;
        "(6)" >:: (fun _ -> assert_equal
            (derivate (P.mul x y))
            (P.add (P.mul (derivate x) y) (P.mul x (derivate y)))) ;
      ] ;
    ]

end

module TestPolynomial (R : Ring) (P : Polynomial with type ft = R.t) = struct

  module TestCommutativeAlgebraImpl = TestCommutativeAlgebra(R)(P)
  module TestPolynomialFunctionsImpl = TestPolynomialFunctions(R)(P)

  let test (x : P.t) (y : P.t) (z : P.t) (f : R.t) (g : R.t) : OUnit.test list =
    (TestPolynomialFunctionsImpl.test_me x y)::(TestCommutativeAlgebraImpl.test x y z f g)

end

module TestEuclidianPolynomial (F : Field) (P : EuclidianPolynomial with type ft = F.t) = struct

  module TestEuclidianAlgebraImpl = TestEuclidianAlgebra(F)(P)
  module TestPolynomialFunctionsImpl = TestPolynomialFunctions(F)(P)

  let test (x : P.t) (y : P.t) (z : P.t) (f : F.t) (g : F.t) : OUnit.test list =
    (TestPolynomialFunctionsImpl.test_me x y)::(TestEuclidianAlgebraImpl.test x y z f g)

end

