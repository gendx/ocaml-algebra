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
open Vector
open ArrayMatrix
open DiagonalMatrix
open TestSquaredMatrix

module VectQ3 = MakeVector(Q)(struct let x = 3 end)
let vect_of_ints = VectQ3.make_of Q.of_int

module MatQ3 = MakeArrayFieldMatrix(Q)(struct let x = 3 end)
module TestMatrixQ3 = TestSquaredFieldMatrix(Q)(MatQ3)

let mat_of_ints = MatQ3.make_of Q.of_int

let testMatrixQ3 =
  "MatrixQ3" >:::
  (TestMatrixQ3.test
    (mat_of_ints [| [| 1 ; 2 ; 3 |] ; [| 4 ; 5 ; 6 |] ; [| 7 ; 8 ; 10 |] |])
    (mat_of_ints [| [| 1 ; 1 ; 2 |] ; [| 3 ; 5 ; 8 |] ; [| 13 ; 21 ; 35 |] |])
    (mat_of_ints [| [| 2 ; 3 ; 5 |] ; [| 7 ; 11 ; 13 |] ; [| 17 ; 19 ; 23 |] |])
    (vect_of_ints [| 1 ; 2 ; 3 |])
    (vect_of_ints [| -1 ; 0 ; 1 |])
    (Q.of_int 11) (Q.of_int 14))
  @ [
    "to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MatQ3.to_string (MatQ3.zero ()))
          "[[0 0 0] [0 0 0] [0 0 0]]") ;
      "(2)" >:: (fun _ -> assert_equal
          (MatQ3.to_string (mat_of_ints [| [| 1 ; 2 ; 3 |] ; [| 4 ; 5 ; 6 |] ; [| 7 ; 8 ; 10 |] |]))
          "[[1 2 3] [4 5 6] [7 8 10]]") ;
    ] ;
  ]

module DiagMatQ3 = MakeDiagonalFieldMatrix(Q)(struct let x = 3 end)
module TestDiagMatrixQ3 = TestSquaredFieldMatrix(Q)(DiagMatQ3)

let diag_mat_of_ints = DiagMatQ3.make_of Q.of_int

let testDiagMatrixQ3 =
  "DiagonalMatrixQ3" >:::
  (TestDiagMatrixQ3.test
    (diag_mat_of_ints [| 1 ; 2 ; 3 |])
    (diag_mat_of_ints [| 1 ; 1 ; 2 |])
    (diag_mat_of_ints [| 2 ; 3 ; 5 |])
    (vect_of_ints [| 1 ; 2 ; 3 |])
    (vect_of_ints [| -1 ; 0 ; 1 |])
    (Q.of_int 11) (Q.of_int 14))
  @ [
    "to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (DiagMatQ3.to_string (DiagMatQ3.zero ()))
          "[[0 0 0] [0 0 0] [0 0 0]]") ;
      "(2)" >:: (fun _ -> assert_equal
          (DiagMatQ3.to_string (diag_mat_of_ints [| 1 ; 2 ; 3 |]))
          "[[1 0 0] [0 2 0] [0 0 3]]") ;
    ] ;
  ]

let tests =
  "Matrix" >:::
  [
    testMatrixQ3 ;
    testDiagMatrixQ3 ;
  ]

