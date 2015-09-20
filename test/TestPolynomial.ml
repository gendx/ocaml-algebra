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
open ListPolynomial
open SparsePolynomial
open TestPolynomialFunctions

module ListPolyQ = MakeEuclidianListPolynomial(Q)
module TestListPolyQ = TestEuclidianPolynomial(Q)(ListPolyQ)

let list_poly_of_ints = ListPolyQ.make_of Q.of_int

let testListPolyQ =
  "ListPolyQ" >:::
  (TestListPolyQ.test
    (list_poly_of_ints [ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ])
    (list_poly_of_ints [ 0 ; 1 ; 1 ; 2 ; 3 ; 5 ; 8 ])
    (list_poly_of_ints [ 1 ; -1 ; 0 ; 1 ; -1 ])
    (Q.of_int 11) (Q.of_int 14))
  @ [
    "to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (ListPolyQ.to_string (ListPolyQ.zero ()))
          "(0)") ;
      "(2)" >:: (fun _ -> assert_equal
          (ListPolyQ.to_string (list_poly_of_ints [ 1 ; 2 ; 3 ; 4 ; 5 ]))
          "(1 + 2x + 3x^2 + 4x^3 + 5x^4)") ;
    ] ;
  ]

module SparsePolyQ = MakeEuclidianSparsePolynomial(Q)
module TestSparsePolyQ = TestEuclidianPolynomial(Q)(SparsePolyQ)

let sparse_poly_of_ints =
  List.fold_left (
    fun sum (a, n) -> SparsePolyQ.add sum (SparsePolyQ.make_monom (Q.of_int a) n)
  ) (SparsePolyQ.zero ())

let testSparsePolyQ =
  "SparsePolyQ" >:::
  (TestSparsePolyQ.test
    (sparse_poly_of_ints [ 1, 7 ; 2, 13 ])
    (sparse_poly_of_ints [ 1, 11 ; 2, 22 ])
    (sparse_poly_of_ints [ 1, 10 ; 3, 20 ])
    (Q.of_int 11) (Q.of_int 14))
  @ [
    "to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (SparsePolyQ.to_string (SparsePolyQ.zero ()))
          "(0)") ;
      "(2)" >:: (fun _ -> assert_equal
          (SparsePolyQ.to_string (sparse_poly_of_ints [ 1, 100 ; 3, 200 ]))
          "(3x^200 + x^100)") ;
    ] ;
  ]

let tests =
  "Polynomial" >:::
  [
    testListPolyQ ;
    testSparsePolyQ ;
  ]

