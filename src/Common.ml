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
open GCD
open Misc

module rec Z :
  EuclidianRing with type t = int
= struct
  
  type t = int
  module GCDImpl = GCD(Z)
  
  let to_string (x : t) : string = string_of_int x
  
  let zero () : t = 0
  let one () : t = 1
  let is_zero (x : t) : bool = (x = 0)
  let is_one (x : t) : bool = (x = 1)
  
  let opposite (x : t) : t = - x
  let of_int (x : int) : t = x
  let mul_int (i : int) (x : t) : t = x * i
  
  let add (x : t) (y : t) : t = x + y
  let sub (x : t) (y : t) : t = x - y
  let mul (x : t) (y : t) : t = x * y
  
  let unit_part (x : t) : t = if x < 0 then -1 else 1
  let normalize (x : t) : (t * t) = if x < 0 then (-x, -1) else (x, 1)
  
  
  let quorem (x : t) (y : t) : (t * t) =
    if x >= 0 then
      (x / y, x mod y)
    else (
      let q = - ((y - 1 - x) / y) in
      (q, x - y * q)
    )
  
  let quo (x : t) (y : t) : t =
    let q, _ = quorem x y in q
  
  let rem (x : t) (y : t) : t =
    let _, r = quorem x y in r
  
  let comp (x : t) (y : t) : bool =
    x < y
  
  let gcd (x : t) (y : t) : t =
    GCDImpl.euclid comp (fst (normalize x)) (fst (normalize y))
  
  let gcd_reduction (x : t) (y : t) : (t * t) =
    let g, _ = normalize (gcd x y) in
    (quo x g, quo y g)
  
  let bezout (x : t) (y : t) : (t * t * t) =
    GCDImpl.bezout comp (fst (normalize x)) (fst (normalize y))
  
end

module Q = MakeFractional(Z)

module QListPoly = MakeEuclidianListPolynomial(Q)
module QSparsePoly = MakeEuclidianSparsePolynomial(Q)
module QMultivarPoly = MakeMultivarPolynomial(Q)

module type ZValue = sig val x: Z.t end

module GF (P : ZValue) = MakeQuotient(Z)(struct type t = Z.t include P end)

module GFListPoly (P : ZValue) = MakeEuclidianListPolynomial(GF(P))
module GFSparsePoly (P : ZValue) = MakeEuclidianSparsePolynomial(GF(P))
module GFMultivarPoly (P : ZValue) = MakeMultivarPolynomial(GF(P))
