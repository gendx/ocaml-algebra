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

open Algebra
open Vector

module type SquaredMatrix = sig
  type ft
  type t
  
  include Algebra with type ft := ft and type t := t
  
  (* Args    : x *)
  (* Returns : y, the transpose of matrix x *)
  val transpose: t -> t
  (* Args    : x *)
  (* Returns : y, the trace of matrix x *)
  val trace: t -> ft

  module Vector : Vector with type ft := ft
  (* Args    : x, y *)
  (* Returns : z, the product of x and y *)
  val mul_vect: t -> Vector.t -> Vector.t
end

module type SquaredFieldMatrix = sig
  include SquaredMatrix

  (* Args    : x *)
  (* Returns : y, the determinant of matrix x *)
  val determinant: t -> ft
  (* Args    : x *)
  (* Returns : y, the inverse of x with respect to multiplication *)
  val inverse: t -> t
end
