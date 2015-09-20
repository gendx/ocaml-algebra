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
open Field
open Errors

module type Fractional = sig
  type rt
  include Field with type t = rt * rt
  
  val make: rt -> rt -> t
  val make_of_int: int -> int -> t
end


module MakeFractional (R : EuclidianRing) = struct
  
  type rt = R.t
  type t = R.t * R.t
  
  
  let to_string ((p, q) : t) : string =
    (R.to_string p) ^
    if (R.is_one q) then
      ""
    else
      ("/" ^ (R.to_string q))
  
  
  let make (p : R.t) (q : R.t) : t =
    let pp, qq = R.gcd_reduction p q in
    let u = R.unit_part q in
    
    (R.mul pp u, R.mul qq u)
  
  let make_of_int (p : int) (q : int) : t =
    make (R.of_int p) (R.of_int q)
  
  
  let zero () : t =
    (R.zero (), R.one ())
  
  let one () : t =
    (R.one (), R.one ())
  
  let is_zero ((p, q) : t) : bool =
    R.is_zero p
  
  let is_one ((p, q) : t) : bool =
    (R.is_one p) && (R.is_one q)
  
  
  let opposite ((p, q) : t) : t =
    (R.opposite p, q)
  
  let inverse ((p, q) : t) : t =
    if R.is_zero p then
      raise Errors.Not_inversible;
    let u = R.unit_part p in
    (R.mul u q, R.mul u p)
  
  let of_int (i : int) : t =
    (R.of_int i, R.one ())
  
  let mul_int (i : int) ((p, q) : t) : t =
    let j = R.of_int i in
    let jj, qq = R.gcd_reduction j q in
    
    (R.mul p jj, qq)
  
  
  let add ((p1, q1) : t) ((p2, q2) : t) : t =
    let a1, a2 = R.gcd_reduction q1 q2 in
    let lcm = R.mul a1 q2 in
    
    make (R.add (R.mul p1 a2) (R.mul p2 a1)) lcm
  
  let sub ((p1, q1) : t) ((p2, q2) : t) : t =
    let a1, a2 = R.gcd_reduction q1 q2 in
    let lcm = R.mul a1 q2 in
    
    make (R.sub (R.mul p1 a2) (R.mul p2 a1)) lcm
  
  let mul ((p1, q1) : t) ((p2, q2) : t) : t =
    let a1, b2 = R.gcd_reduction p1 q2 in
    let a2, b1 = R.gcd_reduction p2 q1 in
    
    (R.mul a1 a2, R.mul b1 b2)
  
  let div ((p1, q1) : t) ((p2, q2) : t) : t =
    if R.is_zero p2 then
      raise Errors.Not_inversible;
    let a1, b2 = R.gcd_reduction p1 p2 in
    let a2, b1 = R.gcd_reduction q2 q1 in
    let u = R.unit_part p2 in
    
    (R.mul u (R.mul a1 a2), R.mul u (R.mul b1 b2))
  
end
