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

module Test = struct
  
  module Q = Common.Q
  module QMultiPoly = Common.QMultivarPoly
  
  let make_qmultipoly_of_int = QMultiPoly.make_of Q.of_int
  
  let test_spoly_impl x y z : bool =
    let px = make_qmultipoly_of_int x in
    let py = make_qmultipoly_of_int y in
    let pz = make_qmultipoly_of_int z in
    
    print_string "spoly(";
    print_string (QMultiPoly.to_string px);
    print_string ", ";
    print_string (QMultiPoly.to_string py);
    print_string ") = ";
    
    let s = (QMultiPoly.spoly px py) in
    print_string (QMultiPoly.to_string s);
    print_newline ();
    
    if QMultiPoly.is_zero (QMultiPoly.sub pz s) then
      true
    else (
      print_string "Unexpected result ! Expected was :";
      print_string (QMultiPoly.to_string pz);
      print_newline ();
      false
    )
  
  let test_spoly () =
    let _ = test_spoly_impl
      [1,[0,1 ; 1,2 ; 2,1] ; -1,[0,1 ; 1,1 ; 2,1]]
      [1,[0,2 ; 1,1 ; 2,1] ; -1,[2,2]]
      [-1,[0,2 ; 1,1 ; 2,1] ; 1,[1,1 ; 2,2]] in
    let _ = test_spoly_impl
      [1,[0,1 ; 1,1] ; 1,[2,3]]
      [1,[2,2] ; -3,[2,1]]
      [3,[0,1 ; 1,1 ; 2,1] ; 1,[2,5]] in
    let _ = test_spoly_impl
      [4,[0,2 ; 2,1] ; -7,[1,2]]
      [1,[0,1 ; 1,1 ; 2,2] ; 3,[0,1 ; 2,4]]
      [-12,[0,2 ; 2,4] ; -7,[1,3 ; 2,1]] in
    let _ = test_spoly_impl
      [1,[0,4 ; 1,1] ; -1,[2,2]]
      [3,[0,1 ; 2,2] ; -1,[1,1]]
      [1,[0,3 ; 1,2] ; -3,[2,4]] in
    ()
  
  let test_multipoly_div_impl px pf pr : bool =
    (*
    let px = make_qmultipoly_of_int x in
    let pf = Array.map make_qmultipoly_of_int f in
    let pr = make_qmultipoly_of_int rem in
    *)
    
    print_string "division(";
    print_string (QMultiPoly.to_string px);
    print_string ", {";
    Array.iter (fun t -> print_string (QMultiPoly.to_string t); print_string ", ") pf;
    print_string "}) = ";
    
    let q, r = QMultiPoly.division_set px pf in
    print_string "{";
    Array.iter (fun t -> print_string (QMultiPoly.to_string t); print_string ", ") q;
    print_string "} + ";
    print_string (QMultiPoly.to_string r);
    print_newline ();
    
    QMultiPoly.is_zero (QMultiPoly.sub pr r)
    
  let test_multipoly_div () =
    let g1 = make_qmultipoly_of_int [4,[0,2 ; 2,1] ; -7,[1,2]] in
    let g2 = make_qmultipoly_of_int [1,[0,1 ; 1,1 ; 2,2] ; 3,[0,1 ; 2,4]] in
    let s = QMultiPoly.spoly g1 g2 in
    let r = make_qmultipoly_of_int [] in
    let _ = test_multipoly_div_impl s (Array.of_list [g1 ; g2]) r in
    ()
    
  let test_groebner_impl x =
    let g = List.map make_qmultipoly_of_int x in
    let base = QMultiPoly.groebner g in
    print_endline "groebner of {";
    List.iter (fun t ->  print_string "\t"; print_string (QMultiPoly.to_string t); print_newline ()) g;
    print_endline "} = {";
    List.iter (fun t ->  print_string "\t"; print_string (QMultiPoly.to_string t); print_newline ()) base;
    print_string "}";
    print_newline ()
    
  let test_groebner () =
    test_groebner_impl [
      [4,[0,2 ; 2,1] ; -7,[1,2]] ;
      [1,[0,1 ; 1,1 ; 2,2] ; 3,[0,1 ; 2,4]] ;
    ];
    test_groebner_impl [
      [1,[0,2 ; 1,2 ; 2,2] ; -1,[]] ;
      [1,[0,2 ; 1,2 ; 2,2] ; 2,[0,1]] ;
      [2,[0,1] ; -3,[1,1] ; -1,[2,1]] ;
    ];
    test_groebner_impl [
      [1,[0,2] ; 1,[1,2] ; 1,[2,2] ; -1,[]] ;
      [1,[0,2] ; 1,[1,2] ; 1,[2,2] ; -2,[0,1]] ;
      [2,[0,1] ; -3,[1,1] ; -1,[2,1]]
    ];
    test_groebner_impl [
      [1,[0,1] ; 1,[1,1] ; 1,[2,1]] ;
      [1,[0,1] ; 1,[2,1]] ;
    ];
    test_groebner_impl [
      [1,[0,2 ; 1,1] ; 1,[0,1] ; 1,[]] ;
      [1,[0,1 ; 1,2] ; 1,[1,1] ; 1,[]] ;
      [1,[0,1] ; -1,[1,1]] ;
    ];
    test_groebner_impl [
      [1,[0,2 ; 1,1] ; -1,[2,3]] ;
      [2,[0,1 ; 1,1] ; -4,[2,1] ; -1,[]] ;
      [1,[2,1] ; -1,[1,2]] ;
      [1,[0,3] ; -4,[1,1 ; 2,1]] ;
    ];
    (*
    (* Hard one ! *)
    test_groebner_impl [
      [1,[3,2] ; 1,[0,2] ; 1,[1,2] ; 1,[2,2]] ;
      [1,[3,2] ; 2,[0,2] ; -1,[0,1 ; 1,1] ; -1,[2,2]] ;
      [1,[3,1] ; 1,[1,3] ; -1,[2,3]] ;
    ]
    *)
    
end
