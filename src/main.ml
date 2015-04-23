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

open Test
open Expression
open DiagonalMatrix
open ArrayMatrix

let () =
  let e = Div (Var "x", Sub (Exp (Var "x"), Const 1)) in
  let de = derivate e in
  let dde = derivate de in
  print_expression e;
  print_newline ();
  print_expression de;
  print_newline ();
  print_expression dde;
  print_newline ();
  
  let module Q = Common.Q in
  let module QPoly = Common.QSparsePoly in
  let make_qpoly_of_int = QPoly.make_of Q.of_int in
  
  let p = make_qpoly_of_int [1 ; 0 ; 1] in
  print_string (QPoly.to_string_var "x" p);
  print_newline ();
  let p2 = QPoly.mul p p in
  print_string (QPoly.to_string_var "x" p2);
  print_newline ();
  
  let module QPolyPoly = SparsePolynomial.MakeSparsePolynomial(QPoly) in
  let make_qpolypoly_of_int = QPolyPoly.make_of make_qpoly_of_int in
  
  (* y = f(x) *)
  let name_y = "exp(-x^2)" in
  (* x' = 1 *)
  let deriv_x = QPoly.one () in
  (* y' = -2xy *)
  let deriv_y = make_qpolypoly_of_int [[] ; [0 ; -2]] in
  
  let deriv_xy = QPolyPoly.derivate deriv_y (QPoly.derivate deriv_x (fun a -> Q.zero ())) in
  
  (*
  let int_poly_ring = SP.make_poly_ring int_ring "x" deriv_y (SP.derivate_poly int_ring) in
  let make_poly_int = SP.make_poly int_ring in
  let make_poly_poly_int = SP.make_poly int_poly_ring in
  *)
  
  let print_xy p = print_string (QPolyPoly.to_string_var name_y p) in
  
  print_xy (make_qpolypoly_of_int [[0 ; 1] ; [1]]);
  print_newline ();
  print_xy (make_qpolypoly_of_int [[] ; [1]]);
  print_newline ();
  print_xy (make_qpolypoly_of_int [[0 ; 1]]);
  print_newline ();
  
  let expr = make_qpolypoly_of_int [[] ; [1]] in
  print_xy expr;
  print_newline ();
  
  let d = ref expr in
  for i = 1 to 10 do
    d := deriv_xy !d;
    print_int i;
    print_string " : ";
    print_xy !d;
    print_newline ();
  done;
  
  let module QMultiPoly = Common.QMultivarPoly in
  
  let make_qmultipoly_of_int = QMultiPoly.make_of Q.of_int in
  let print_xy_mp p = print_string (QMultiPoly.to_string_names (Array.of_list ["x" ; "exp(x)"]) p) in
  let deriv = Array.of_list [
    make_qmultipoly_of_int [1, []] ; (* x' = 1 *)
    make_qmultipoly_of_int [1, [1, 1]] ; (* y' = y *)
  ] in
  let derivate_qmultipoly = QMultiPoly.derivate deriv in
  
  let expr2 = make_qmultipoly_of_int [1, [(0, 1) ; (1, 2)]] in (* x*exp(x)^2 *)
  print_xy_mp expr2;
  print_newline ();
  
  let d2 = ref expr2 in
  for i = 1 to 10 do
    d2 := derivate_qmultipoly !d2;
    print_int i;
    print_string " : ";
    print_xy_mp !d2;
    print_newline ();
  done;
  
  Test.test_spoly ();
  
  Test.test_multipoly_div ();
  
  Test.test_groebner ();
  
  let prime = 3 in
  let module GF = Common.GF(struct let x = prime end) in
  let a = GF.of_int 10 in
  print_string (GF.to_string a);
  print_newline ();
  print_string (GF.to_string (GF.inverse a));
  print_newline ();
  print_string (GF.to_string (GF.opposite a));
  print_newline ();
  
  let module Z = Common.Z in
  print_string (Z.to_string (Z.one ()));
  print_newline ();
  
  print_string (Q.to_string (Q.make_of_int 15 (-35)));
  print_newline ();
  
  print_newline ();

  let module GFPoly = ListPolynomial.MakeEuclidianListPolynomial(GF) in

  (**)
  let make_qpoly_of_int = QPoly.make_of Q.of_int in
  let p = make_qpoly_of_int [5 ; 0 ; 0 ; 1] in
  print_string (QPoly.to_string p);
  print_newline ();
  (**)
  
  let make_poly_of_int = GFPoly.make_of GF.of_int in
  let p = make_poly_of_int [1 ; 2 ; 0 ; 1] in
  print_string (GFPoly.to_string p);
  print_newline ();
  
  let module GF3 = Quotient.MakeQuotient(GFPoly)(struct type t = GFPoly.t let x = p end) in
  let a = GF3.make (make_poly_of_int [0 ; 1]) in
  print_string (GF3.to_string a);
  print_newline ();
  
  (**)
  let aa = make_qpoly_of_int [0 ; 1] in
  let c = QPoly.mul aa aa in
  print_string (QPoly.to_string c);
  print_newline ();
  
  let pp = make_qpoly_of_int [1 ; 2 ; 0 ; 1] in
  let q, r = QPoly.quorem c pp in
  print_string (QPoly.to_string q);
  print_newline ();
  print_string (QPoly.to_string r);
  print_newline ();
  
  print_string (QPoly.to_string (QPoly.mul pp q));
  print_newline ();
  (**)
  
  (**)
  let a0, b0, g0 = QPoly.bezout aa pp in
  let a1 = QPoly.quo a0 g0 in
  let b1 = QPoly.quo b0 g0 in
  print_string (QPoly.to_string a1);
  print_newline ();
  print_string (QPoly.to_string b1);
  print_newline ();
  print_string (QPoly.to_string (QPoly.add (QPoly.mul a1 aa) (QPoly.mul b1 pp)));
  print_newline ();
  
  print_newline ();
  (**)
  
  let b = ref a in
  for i = 2 to 30 do
    b := GF3.mul !b a;
    print_int i;
    print_string " : ";
    print_string (GF3.to_string !b);
    print_newline ();
  done;
  
  print_newline ();
  
  for i = 2 to 30 do
    b := GF3.div !b a;
    print_int i;
    print_string " : ";
    print_string (GF3.to_string !b);
    print_newline ();
  done;
  
  print_newline ();
