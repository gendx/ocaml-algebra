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
  QPoly.print_var "x" p;
  print_newline ();
  let p2 = QPoly.mul p p in
  QPoly.print_var "x" p2;
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
  
  let print_xy = QPolyPoly.print_var name_y in
  
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
  let print_xy_mp = QMultiPoly.print_names (Array.of_list ["x" ; "exp(x)"]) in
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
  GF.print a;
  print_newline ();
  GF.print (GF.inverse a);
  print_newline ();
  GF.print (GF.opposite a);
  print_newline ();
  
  let module Z = Common.Z in
  Z.print (Z.one ());
  print_newline ();
  
  Q.print (Q.make_of_int 15 (-35));
  print_newline ();
  
  print_newline ();

  let module GFPoly = ListPolynomial.MakeEuclidianListPolynomial(GF) in

  (*
  let module Qpoly = Structures.Qpoly in
  let make_qpoly_of_int = Qpoly.make_of Q.of_int in
  let p = make_qpoly_of_int [5 ; 0 ; 0 ; 1] in
  Qpoly.print p;
  print_newline ();
  *)
  
  let make_poly_of_int = GFPoly.make_of GF.of_int in
  let p = make_poly_of_int [1 ; 2 ; 0 ; 1] in
  GFPoly.print p;
  print_newline ();
  
  let module GF3 = Quotient.MakeQuotient(GFPoly)(struct type t = GFPoly.t let x = p end) in
  let a = GF3.make (make_poly_of_int [0 ; 1]) in
  GF3.print a;
  print_newline ();
  
  (*
  let c = Qpoly.mul a a in
  Qpoly.print c;
  print_newline ();
  
  let q, r = Qpoly.quorem c p in
  Qpoly.print q;
  print_newline ();
  Qpoly.print r;
  print_newline ();
  
  Qpoly.print (Qpoly.mul p q);
  print_newline ();
  *)
  
  (*
  let a0, b0, g0 = Qpoly.bezout_pair a p in
  let a1 = Qpoly.quo a0 g0 in
  let b1 = Qpoly.quo b0 g0 in
  Qpoly.print a1;
  print_newline ();
  Qpoly.print b1;
  print_newline ();
  Qpoly.print (Qpoly.add (Qpoly.mul a1 a) (Qpoly.mul b1 p));
  print_newline ();
  
  print_newline ();
  *)
  
  let b = ref a in
  for i = 2 to 30 do
    b := GF3.mul !b a;
    print_int i;
    print_string " : ";
    GF3.print !b;
    print_newline ();
  done;
  
  print_newline ();
  
  for i = 2 to 30 do
    b := GF3.div !b a;
    print_int i;
    print_string " : ";
    GF3.print !b;
    print_newline ();
  done;
  
  print_newline ();
