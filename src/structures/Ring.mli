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

open Group

module type Ring = sig
  include AdditiveGroup
  
  (* Returns : x, the one element *)
  (*               i.e. neutral element with respect to multiplication *)
  val one: unit -> t
  (* Args    : x *)
  (* Returns : true if x is the one element *)
  (*           false otherwise *)
  val is_one: t -> bool
  
  (* Args    : n *)
  (* Returns : x, the addition of the one element n times *)
  val of_int: int -> t
  
  (* Args    : x, y *)
  (* Returns : z = x.y, defined by multiplication *)
  val mul: t -> t -> t
end
