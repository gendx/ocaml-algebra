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
open Group

module TestGroup (G : AdditiveGroup) = struct

  (* Args    : three non-zero elements of group G *)
  (* Returns : a test suite for group properties of G *)
  let test_me (x : G.t) (y : G.t) (z : G.t) : OUnit.test =
    ("AdditiveGroup(" ^ (G.to_string x) ^ ", " ^ (G.to_string y) ^ ", " ^ (G.to_string z) ^ ")") >:::
    [
      "is_zero" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (G.is_zero (G.zero ()))
            true) ;
        "(2)" >:: (fun _ -> assert_equal
            (G.is_zero x)
            false) ;
      ] ;

      "opposite" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (G.opposite (G.zero ()))
            (G.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (G.opposite (G.opposite x))
            x) ;
      ] ;
      "mul_int" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (G.mul_int 2 (G.zero ()))
            (G.zero ())) ;
        "(2)" >:: (fun _ -> assert_equal
            (G.mul_int 0 x)
            (G.zero ())) ;
        "(3)" >:: (fun _ -> assert_equal
            (G.mul_int 1 x)
            x) ;
        "(4)" >:: (fun _ -> assert_equal
            (G.mul_int 2 x)
            (G.add x x)) ;
        "(5)" >:: (fun _ -> assert_equal
            (G.mul_int 3 x)
            (G.add x (G.add x x))) ;
        "(6)" >:: (fun _ -> assert_equal
            (G.mul_int (-1) x)
            (G.opposite x)) ;
        "(7)" >:: (fun _ -> assert_equal
            (G.mul_int (-5) x)
            (G.opposite (G.mul_int 5 x))) ;
        "(8)" >:: (fun _ -> assert_equal
            (G.mul_int 3 (G.mul_int 4 x))
            (G.mul_int 12 x)) ;
      ] ;

      "add" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (G.add x (G.zero ()))
            x) ;
        "(2)" >:: (fun _ -> assert_equal
            (G.add (G.zero ()) x)
            x) ;
        "(3)" >:: (fun _ -> assert_equal
            (G.add x (G.add y z))
            (G.add (G.add x y) z)) ;
        "(4)" >:: (fun _ -> assert_equal
            (G.add x y)
            (G.add y x)) ;
      ] ;
      "sub" >:::
      [
        "(1)" >:: (fun _ -> assert_equal
            (G.sub x (G.zero ()))
            x) ;
        "(2)" >:: (fun _ -> assert_equal
            (G.sub (G.zero ()) x)
            (G.opposite x)) ;
        "(3)" >:: (fun _ -> assert_equal
            (G.sub x x)
            (G.zero ())) ;
        "(4)" >:: (fun _ -> assert_equal
            (G.sub x (G.add y z))
            (G.sub (G.sub x y) z)) ;
        "(5)" >:: (fun _ -> assert_equal
            (G.sub x y)
            (G.opposite (G.sub y x))) ;
      ] ;
    ]

  let test (x : G.t) (y : G.t) (z : G.t) : OUnit.test list =
    [test_me x y z]

end

