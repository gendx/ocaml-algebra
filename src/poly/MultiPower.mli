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

module MultiPower : sig
  type t = ((int * int) list) * int
  
  (* Args    : x *)
  (* Returns : a printable string of x *)
  val to_string: t -> string
  (* Args    : f, n *)
  (* Returns : a printable string of x as a monomial with variable names given by f *)
  val power_to_string: (int -> string) -> t -> string
  
  (* Args    : x *)
  (* Returns : p, the monomial where the non-null power n of variable y is given by *)
  (*               element (y, n) in the list *)
  val make: (int * int) list -> t
  
  (* Returns : x, the zero element *)
  val zero: unit -> t
  (* Args    : x *)
  (* Returns : true if x is zero *)
  (*           false otherwise *)
  val is_zero: t -> bool
  
  (* Args    : x, y *)
  (* Returns : the following result according to the lexicographic order *)
  (*                 0 if x = y *)
  (*               < 0 if x < y *)
  (*               > 0 if x > y *)
  val compare_lex: t -> t -> int
  (* Args    : x, y *)
  (* Returns : the following result according to the graded lexicographic order *)
  (*                 0 if x = y *)
  (*               < 0 if x < y *)
  (*               > 0 if x > y *)
  val compare_grlex: t -> t -> int
  (* Args    : x, y *)
  (* Returns : the following result according to a compatible total order *)
  (*                 0 if x = y *)
  (*               < 0 if x < y *)
  (*               > 0 if x > y *)
  val compare: t -> t -> int

  (* Args    : x, y *)
  (* Returns : z, the element-wise addition of powers x and y *)
  val add: t -> t -> t
  (* Args    : (v, n), x *)
  (* Returns : y, the multipower x where the power of variable v has increased by n *)
  val add_var: (int * int) -> t -> t
  
  (* Args    : x, y *)
  (* Returns : g, the greatest common divisor of the monomials (x, y) *)
  val gcd: t -> t -> t
  (* Args    : x, y *)
  (* Returns : (a, b) such that : *)
  (*               a = x - g *)
  (*               b = y - g *)
  (*               g is the greatest common divisor of (x, y) *)
  val gcd_reduction: t -> t -> (t * t)
end
