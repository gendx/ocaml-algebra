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
open Field
open TestRing

module TestField (F : Field) = struct

  (* Args    : three non-zero elements of field F *)
  (* Returns : a test suite for field properties of F *)
  let test_me (x : F.t) (y : F.t) (z : F.t) : OUnit.test =
    ("Field(" ^ (F.to_string x) ^ ", " ^ (F.to_string y) ^ ", " ^ (F.to_string z) ^ ")") >:::
    [
      "inverse" >:::
      [
        "(1)" >:: (fun _ -> assert_raises
            Errors.Not_inversible
            (fun () -> (F.inverse (F.zero ())))) ;
        "(2)" >:: (fun _ -> assert_equal
            (F.inverse (F.one ()))
            (F.one ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (F.inverse (F.inverse x))
            x) ;
      ] ;
      "div" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (F.div x (F.one ()))
            x) ;
        "(2)" >:: (fun _ -> assert_equal
            (F.div (F.one ()) x)
            (F.inverse x)) ;
        "(3)" >:: (fun _ -> assert_equal
            (F.div x x)
            (F.one ())) ;
        "(4)" >:: (fun _ -> assert_equal
            (F.div x (F.mul y z))
            (F.div (F.div x y) z)) ;
        "(5)" >:: (fun _ -> assert_equal
            (F.div x y)
            (F.inverse (F.div y x))) ;
        "(6)" >:: (fun _ -> assert_raises
            Errors.Not_inversible
            (fun () -> (F.div x (F.zero ())))) ;
        "(7)" >:: (fun _ -> assert_equal
            (F.div (F.zero ()) x)
            (F.zero ())) ;
      ] ;
    ]

  module TestCommutativeRingImpl = TestCommutativeRing(F)

  let test (x : F.t) (y : F.t) (z : F.t) : OUnit.test list =
    (test_me x y z)::(TestCommutativeRingImpl.test x y z)

end

