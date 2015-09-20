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
open EuclidianRing
open TestRing

module TestEuclidianRing (R : EuclidianRing) = struct

  let normalize (x : R.t) : R.t =
    fst (R.normalize x)

  (* Args    : three non-zero elements of euclidian ring R *)
  (* Returns : a test suite for euclidian ring properties of R *)
  let test_me (x : R.t) (y : R.t) (z : R.t) : OUnit.test =
    ("EuclidianRing(" ^ (R.to_string x) ^ ", " ^ (R.to_string y) ^ ", " ^ (R.to_string z) ^ ")") >:::
    [
      "unit_part" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (R.unit_part (R.one ()))
            (R.one ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (R.unit_part (R.zero ()))
            (R.one ())) ;
      ] ;
      "normalize" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (snd (R.normalize x))
            (R.unit_part x)) ;
        "(2)" >:: (fun _ -> assert_equal
            (R.unit_part (fst (R.normalize x)))
            (R.one ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (fst (R.normalize x))
            (R.mul x (R.unit_part x))) ;
        "(4)" >:: (fun _ -> assert_equal
            (R.normalize (normalize x))
            (normalize x, R.one ())) ;
      ] ;

      "quorem" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (fst (R.quorem x y))
            (R.quo x y)) ;
        "(2)" >:: (fun _ -> assert_equal
            (snd (R.quorem x y))
            (R.rem x y)) ;
        "(3)" >:: (fun _ -> assert_equal
            (R.add (R.mul (R.quo x y) y) (R.rem x y))
            x) ;
      ] ;
      "quo" >:::
      [
        "(1)" >:: (fun _ -> assert_raises
            Errors.Not_inversible
            (fun () -> R.quo x (R.zero ()))) ;
        "(2)" >:: (fun _ -> assert_equal
            (R.quo (R.zero ()) x)
            (R.zero ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (R.quo (R.mul x y) y)
            x) ;
      ] ;
      "rem" >:::
      [
        "(1)" >:: (fun _ -> assert_raises
            Errors.Not_inversible
            (fun () -> R.rem x (R.zero ()))) ;
        "(2)" >:: (fun _ -> assert_equal
            (R.rem (R.zero ()) x)
            (R.zero ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (R.rem (R.mul x y) y)
            (R.zero ())) ;
        "(4)" >:: (fun _ -> assert_equal
            (R.rem (R.add x y) z)
            (R.rem (R.add (R.rem x z) (R.rem y z)) z)) ;
        "(5)" >:: (fun _ -> assert_equal
            (R.rem (R.sub x y) z)
            (R.rem (R.sub (R.rem x z) (R.rem y z)) z)) ;
        "(6)" >:: (fun _ -> assert_equal
            (R.rem (R.mul x y) z)
            (R.rem (R.mul (R.rem x z) (R.rem y z)) z)) ;
      ] ;
      "gcd" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (normalize (R.gcd (R.zero ()) x))
            (normalize x)) ;
        "(2)" >:: (fun _ -> assert_equal
            (normalize (R.gcd x (R.zero ())))
            (normalize x)) ;
        "(3)" >:: (fun _ -> assert_equal
            (R.rem x (R.gcd x y))
            (R.zero ())) ;
      ] ;
      "gcd_reduction" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (let xx, yy = R.gcd_reduction x y in
            normalize (R.gcd xx yy))
            (R.one ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (let xx, yy = R.gcd_reduction x y in
            R.mul (normalize (R.gcd x y)) xx)
            x) ;
        "(3)" >:: (fun _ -> assert_equal
            (let xx, yy = R.gcd_reduction x y in
            R.mul (normalize (R.gcd x y)) yy)
            y) ;
      ] ;
      "bezout" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (let a, b, g = R.bezout x y in
            R.sub (R.add (R.mul a x) (R.mul b y)) g)
            (R.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (let _, _, g = R.bezout x y in
            normalize g)
            (normalize (R.gcd x y))) ;
      ] ;
    ]

  module TestCommutativeRingImpl = TestCommutativeRing(R)

  let test (x : R.t) (y : R.t) (z : R.t) : OUnit.test list =
    (test_me x y z)::(TestCommutativeRingImpl.test x y z)

end

