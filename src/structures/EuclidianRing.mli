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

open Ring

module type EuclidianRing = sig
  include Ring
  
  (* Args    : x *)
  (* Returns : u, a unit element such that : *)
  (*               x.u is normalized *)
  val unit_part: t -> t
  (* Args    : x *)
  (* Returns : (y, u) such that : *)
  (*               y = normalized(x) *)
  (*               y = x.u *)
  val normalize: t -> (t * t)
  
  (* Args    : x, y *)
  (* Returns : (q, r) such that : *)
  (*               x = q.y + r is the euclidian division of x by y *)
  val quorem: t -> t -> (t * t)
  (* Args    : x, y *)
  (* Returns : q, the quotient of the euclidian division of x by y *)
  val quo: t -> t -> t
  (* Args    : x, y *)
  (* Returns : r, the remainder of the euclidian division of x by y *)
  val rem: t -> t -> t
  (* Args    : x, y *)
  (* Returns : g, a greatest common divisor of (x, y) *)
  val gcd: t -> t -> t
  (* Args    : x, y *)
  (* Returns : (qx, qy) such that : *)
  (*               g is the normalized greatest common divisor of (x, y) *)
  (*               x = g.qx *)
  (*               y = g.qy *)
  val gcd_reduction: t -> t -> (t * t)
  (* Args    : x, y *)
  (* Returns : (a, b, g) such that : *)
  (*               g is a greatest common divisor of (x, y) *)
  (*               a.x + b.y = g *)
  val bezout: t -> t -> (t * t * t)
end
