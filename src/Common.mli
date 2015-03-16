open EuclidianRing
open Fractional
open Quotient
open ListPolynomial
open SparsePolynomial
open MultivarPolynomial
open Misc

module Z : EuclidianRing with type t = int
module Q : Fractional with type rt = Z.t

module QListPoly : EuclidianListPolynomial with type ft = Q.t
module QSparsePoly : EuclidianSparsePolynomial with type ft = Q.t
module QMultivarPoly : MultivarPolynomial with type ft = Q.t

module type ZValue = sig val x: Z.t end
module GF (P : ZValue) : Quotient with type rt = Z.t

module GFListPoly (P : ZValue) : EuclidianListPolynomial with type ft = GF(P).t
module GFSparsePoly (P : ZValue) : EuclidianSparsePolynomial with type ft = GF(P).t
module GFMultivarPoly (P : ZValue) : MultivarPolynomial with type ft = GF(P).t

(* module BigZ : Ring with type t = big_int *)
