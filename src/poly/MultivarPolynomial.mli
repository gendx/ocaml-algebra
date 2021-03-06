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

open MultiPower
open Algebra
open Field

module type MultivarPolynomial = sig
  type ft
  include Algebra with type ft := ft and type t = (ft * MultiPower.t) list
  
  (* Args    : f, x *)
  (* Returns : a printable string of x with variable names given by f *)
  val to_string_with: (int -> string) -> t -> string
  (* Args    : f, x *)
  (* Returns : a printable string of x with variable names given by f *)
  val to_string_names: string array -> t -> string
  
  (* Args    : x *)
  (* Returns : p, the multivariate polynomial whose coefficients are the elements of x *)
  (*               given by tuples (coefficient, multipower) *)
  val make: (ft * ((int * int) list)) list -> t
  (* Args    : f, x *)
  (* Returns : p, the multivariate polynomial whose coefficients are given by tuples (a, n) *)
  (*               where a = f b *)
  (*               and (b, n) are the tuples of x *)
  val make_of: ('a -> ft) -> ('a * ((int * int) list)) list -> t
  
  (* Args    : f, p *)
  (* Returns : q, the derivative of p such that : *)
  (*               f is the derivation function of the variables *)
  val derivate: t array -> t -> t
  
  (* Args    : x *)
  (* Returns : y = x.u such that : *)
  (*               u is a unit element *)
  (*               y is monic *)
  val normalize: t -> t
  (* Args    : x, y *)
  (* Returns : z = (lcm.x / lx) - (lcm.y / ly) such that : *)
  (*               lx is the leading term of x *)
  (*               ly is the leading term of y *)
  (*               lcm is the least common multiple of lx and ly *)
  val spoly: t -> t -> t
  (* Args    : x, y *)
  (* Returns : q, r such that *)
  (*               x = q.y + r *)
  (*               r is completely reduced with respect to y *)
  val division: t -> t -> (t * t)
  (* Args    : x, y *)
  (* Returns : q, r such that *)
  (*               x = sum(q_i.y_i) + r *)
  (*               r is completely reduced with respect to y *)
  val division_set: t -> t array -> (t array * t)
  (* Args    : x *)
  (* Returns : a Groebner basis of x *)
  val groebner: t list -> t list
end


module MakeMultivarPolynomial (F : Field) :
  MultivarPolynomial with type ft = F.t
