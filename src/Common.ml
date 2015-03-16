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
  
  let print (x : t) : unit = print_int x
  
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
