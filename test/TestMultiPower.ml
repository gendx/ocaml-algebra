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
open MultiPower

let generate_name i =
  String.make 1 (Char.chr ((Char.code 'a') + (i mod 26)))


let tests =
  "MultiPower" >:::
  [
    "make" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.make [])
          ([], 0)) ;
      "(2)" >:: (fun _ -> assert_equal
          (MultiPower.make [0,1])
          ([0,1], 1)) ;
      "(3)" >:: (fun _ -> assert_equal
          (MultiPower.make [0,1 ; 1,2])
          ([0,1 ; 1,2], 3)) ;
      "(4)" >:: (fun _ -> assert_equal
          (MultiPower.make [5,1 ; 3,3])
          ([3,3 ; 5,1], 4)) ;
      "(5)" >:: (fun _ -> assert_equal
          (MultiPower.make [4,0])
          ([], 0)) ;
      "(6)" >:: (fun _ -> assert_equal
          (MultiPower.make [4,0 ; 7,0 ; 2,1])
          ([2,1], 1)) ;
    ] ;
    "zero" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.zero ())
          ([], 0)) ;
    ] ;
    "is_zero" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.is_zero (MultiPower.zero ()))
          true) ;
      "(2)" >:: (fun _ -> assert_equal
          (MultiPower.is_zero (MultiPower.make [0,1]))
          false) ;
    ] ;

    "to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.to_string (MultiPower.zero ()))
          "{}") ;
      "(2)" >:: (fun _ -> assert_equal
          (MultiPower.to_string (MultiPower.make [0,1]))
          "{(0, 1)}") ;
      "(3)" >:: (fun _ -> assert_equal
          (MultiPower.to_string (MultiPower.make [7,2 ; 8,5]))
          "{(7, 2), (8, 5)}") ;
    ] ;
    "power_to_string" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.power_to_string generate_name (MultiPower.zero ()))
          "") ;
      "(2)" >:: (fun _ -> assert_equal
          (MultiPower.power_to_string generate_name (MultiPower.make [0,1]))
          "a") ;
      "(3)" >:: (fun _ -> assert_equal
          (MultiPower.power_to_string generate_name (MultiPower.make [2,2 ; 3,5]))
          "c^2d^5") ;
    ] ;

    "compare_lex" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.compare_lex (MultiPower.zero ()) (MultiPower.zero ()))
          0) ;
      "(2)" >:: (fun _ -> assert_equal
          (MultiPower.compare_lex (MultiPower.make [0,2 ; 2,3]) (MultiPower.make [0,2 ; 2,3]))
          0) ;
      "(3)" >:: (fun _ -> assert_equal
          (MultiPower.compare_lex (MultiPower.zero ()) (MultiPower.make [0,1]))
          ~cmp:(<) 0) ;
      "(4)" >:: (fun _ -> assert_equal
          (MultiPower.compare_lex (MultiPower.make [0,1]) (MultiPower.zero ()))
          ~cmp:(>) 0) ;
      "(5)" >:: (fun _ -> assert_equal
          (MultiPower.compare_lex (MultiPower.make [0,2]) (MultiPower.make [2,5]))
          ~cmp:(>) 0) ;
      "(6)" >:: (fun _ -> assert_equal
          (MultiPower.compare_lex (MultiPower.make [0,2 ; 2,3]) (MultiPower.make [0,2 ; 1,1]))
          ~cmp:(<) 0) ;
    ] ;
    "compare_grlex" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.compare_grlex (MultiPower.zero ()) (MultiPower.zero ()))
          0) ;
      "(2)" >:: (fun _ -> assert_equal
          (MultiPower.compare_grlex (MultiPower.make [0,2 ; 2,3]) (MultiPower.make [0,2 ; 2,3]))
          0) ;
      "(3)" >:: (fun _ -> assert_equal
          (MultiPower.compare_grlex (MultiPower.zero ()) (MultiPower.make [0,1]))
          ~cmp:(<) 0) ;
      "(4)" >:: (fun _ -> assert_equal
          (MultiPower.compare_grlex (MultiPower.make [0,1]) (MultiPower.zero ()))
          ~cmp:(>) 0) ;
      "(5)" >:: (fun _ -> assert_equal
          (MultiPower.compare_grlex (MultiPower.make [0,2]) (MultiPower.make [2,5]))
          ~cmp:(<) 0) ;
      "(6)" >:: (fun _ -> assert_equal
          (MultiPower.compare_grlex (MultiPower.make [0,2 ; 2,3]) (MultiPower.make [0,2 ; 1,1]))
          ~cmp:(>) 0) ;
    ] ;

    "add" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.add (MultiPower.make [1,2]) (MultiPower.make [3,4]))
          (MultiPower.make [1,2 ; 3,4])) ;
      "(2)" >:: (fun _ -> assert_equal
          (MultiPower.add (MultiPower.make [1,2 ; 2,3 ; 3,4 ; 4,5]) (MultiPower.make [2,4 ; 6,8]))
          (MultiPower.make [1,2 ; 2,7 ; 3,4 ; 4,5 ; 6,8])) ;
    ] ;
    "add_var" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.add_var (3,2) (MultiPower.make [3,4]))
          (MultiPower.make [3,6])) ;
      "(2)" >:: (fun _ -> assert_equal
          (MultiPower.add_var (3,-3) (MultiPower.make [3,4]))
          (MultiPower.make [3,1])) ;
      "(3)" >:: (fun _ -> assert_equal
          (MultiPower.add_var (3,-4) (MultiPower.make [3,4]))
          (MultiPower.zero ())) ;
      "(4)" >:: (fun _ -> assert_equal
          (MultiPower.add_var (3,-10) (MultiPower.make [3,4]))
          (MultiPower.zero ())) ;
      "(5)" >:: (fun _ -> assert_equal
          (MultiPower.add_var (1,2) (MultiPower.make [0,1]))
          (MultiPower.make [0,1 ; 1,2])) ;
      "(6)" >:: (fun _ -> assert_equal
          (MultiPower.add_var (1,1) (MultiPower.make [3,5]))
          (MultiPower.make [1,1 ; 3,5])) ;
      "(7)" >:: (fun _ -> assert_equal
          (MultiPower.add_var (1,-1) (MultiPower.make [0,1]))
          (MultiPower.make [0,1])) ;
    ] ;

    "gcd" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.gcd (MultiPower.make [1,2]) (MultiPower.make [3,4]))
          (MultiPower.zero ())) ;
      "(2)" >:: (fun _ -> assert_equal
          (MultiPower.gcd (MultiPower.make [1,2 ; 3,4]) (MultiPower.make [1,3 ; 2,4]))
          (MultiPower.make [1,2])) ;
      "(3)" >:: (fun _ -> assert_equal
          (MultiPower.gcd (MultiPower.make [1,2 ; 3,4 ; 5,6]) (MultiPower.make [1,3 ; 2,4 ; 5,3 ; 6,7]))
          (MultiPower.make [1,2 ; 5,3])) ;
    ] ;
    "gcd_reduction" >:::
    [
      "(1)" >:: (fun _ -> assert_equal
          (MultiPower.gcd_reduction (MultiPower.make [1,2]) (MultiPower.make [3,4]))
          (MultiPower.make [1,2], MultiPower.make [3,4])) ;
      "(2)" >:: (fun _ -> assert_equal
          (MultiPower.gcd_reduction (MultiPower.make [1,2 ; 3,4]) (MultiPower.make [1,3 ; 2,4]))
          (MultiPower.make [3,4], MultiPower.make [1,1 ; 2,4])) ;
      "(3)" >:: (fun _ -> assert_equal
          (MultiPower.gcd_reduction (MultiPower.make [1,2 ; 3,4 ; 5,6]) (MultiPower.make [1,3 ; 2,4 ; 5,3 ; 6,7]))
          (MultiPower.make [3,4 ; 5,3], MultiPower.make [1,1 ; 2,4 ; 6,7])) ;
    ] ;
  ]

