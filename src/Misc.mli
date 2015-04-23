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

module Misc : sig
  (* Args    : f, g, h, a, b *)
  (* Returns : c the list such that each element is : *)
  (*               (f a_i b_i) if i < min(len(a), len(b)) *)
  (*               (g a_i) if len(b) <= i < len(a) *)
  (*               (f b_i) if len(a) <= i < len(b) *)
  val map3: ('a -> 'b -> 'c) -> ('a -> 'c) -> ('b -> 'c) -> 'a list -> 'b list -> 'c list
  (* Args    : f, x, y *)
  (* Returns : the lexicographical comparison of x and y *)
  (*               according to the comparison function f *)
  val lexcompare: ('a -> 'a -> int) -> ('a list) -> ('a list) -> int
  (* Args    : x *)
  (* Returns : the list of pairs {y, z} with y, z are distinct elements of x *)
  val pairs: ('a list) -> (('a * 'a) list)
  (* Args    : s, x *)
  (* Returns : a printable string of the monomial x with variable name s *)
  val mono_to_string: string -> int -> string
end

module type Value = sig
  type t
  val x: t
end
