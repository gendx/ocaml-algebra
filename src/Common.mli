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
