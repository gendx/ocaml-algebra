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

module GCD (R : EuclidianRing) : sig
  (* Args    : f, x, y *)
  (* Returns : z = gcd(x, y) given by Euclid's algorithm where : *)
  (*               f is a partial order on the ring *)
  val euclid: (R.t -> R.t -> bool) -> R.t -> R.t -> R.t
  (* Args    : f, x, y *)
  (* Returns : (a, b, z) where : *)
  (*               z = gcd(x, y) *)
  (*               a.x + b.y = z *)
  (*               f is a partial order on the ring *)
  val bezout: (R.t -> R.t -> bool) -> R.t -> R.t -> (R.t * R.t * R.t)
end
