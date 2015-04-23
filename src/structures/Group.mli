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

module type AdditiveGroup = sig
  type t
  
  (* Args    : x *)
  (* Returns : a printable string of x *)
  val to_string: t -> string
  
  (* Returns : x, the zero element *)
  (*               i.e. neutral element with respect to addition *)
  val zero: unit -> t
  (* Args    : x *)
  (* Returns : true if x is the zero element *)
  (*           false otherwise *)
  val is_zero: t -> bool
  
  (* Args    : x *)
  (* Returns : y, the opposite of x with respect to addition *)
  val opposite: t -> t
  (* Args    : n, x *)
  (* Returns : y, the addition of x n times *)
  val mul_int: int -> t -> t
  
  (* Args    : x, y *)
  (* Returns : z = x + y, defined by addition *)
  val add: t -> t -> t
  (* Args    : x, y *)
  (* Returns : z = x + (-y), where -y is the additive opposite of y *)
  val sub: t -> t -> t
end
