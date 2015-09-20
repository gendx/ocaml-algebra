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

module type DiagonalMatrixFunctions = sig
  type ft
  type t

  (* Args    : x *)
  (* Returns : m, the matrix whose diagonal coefficients are the elements of x *)
  val make: ft array -> t
  (* Args    : f, x *)
  (* Returns : m, the matrix whose diagonal coefficients are the elements of x mapped by f *)
  val make_of: ('a -> ft) -> 'a array -> t
end

module type DiagonalMatrix = sig
  type ft
  include SquaredMatrix with type ft := ft and type t = ft array
  include DiagonalMatrixFunctions with type ft := ft and type t := t
end

module type DiagonalFieldMatrix = sig
  type ft
  include SquaredFieldMatrix with type ft := ft and type t = ft array
  include DiagonalMatrixFunctions with type ft := ft and type t := t
end


module MakeDiagonalMatrix (R : Ring) (Size : IntValue) :
  DiagonalMatrix with type ft = R.t

module MakeDiagonalFieldMatrix (F : Field) (Size : IntValue) :
  DiagonalFieldMatrix with type ft = F.t

