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

open Matrix
open Ring
open Field
open Vector

module type ArrayMatrixFunctions = sig
  type ft
  type t
  type vect_t
  
  (* Args    : x *)
  (* Returns : m, the matrix whose coefficients are the elements of x *)
  val make: ft array array -> t
  (* Args    : f, x *)
  (* Returns : m, the matrix whose coefficients are the elements of x mapped by f *)
  val make_of: ('a -> ft) -> 'a array array -> t
  (* Args    : x, y *)
  (* Returns : m, the matrix given by the tensorial product of x and y *)
  (*               i.e. m = x.y^T *)
  val of_vects: vect_t -> vect_t -> t
end

module type ArrayMatrix = sig
  type ft
  include SquaredMatrix with type ft := ft and type t = ft array array
  include ArrayMatrixFunctions with type ft := ft and type t := t and type vect_t := Vector.t
end

module type ArrayFieldMatrix = sig
  type ft
  include SquaredFieldMatrix with type ft := ft and type t = ft array array
  include ArrayMatrixFunctions with type ft := ft and type t := t and type vect_t := Vector.t
end


module MakeArrayMatrix (R : Ring) (Size : IntValue) :
  ArrayMatrix with type ft = R.t

module MakeArrayFieldMatrix (F : Field) (Size : IntValue) :
  ArrayFieldMatrix with type ft = F.t

