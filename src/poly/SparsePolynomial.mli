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

open Polynomial
open Ring
open Field

module type SparsePolynomial = sig
  type ft
  include Polynomial with type ft := ft and type t = (ft * int) list
end

module type EuclidianSparsePolynomial = sig
  type ft
  include EuclidianPolynomial with type ft := ft and type t = (ft * int) list
end


module MakeSparsePolynomial (R : Ring) :
  SparsePolynomial with type ft = R.t

module MakeEuclidianSparsePolynomial (F : Field) :
  EuclidianSparsePolynomial with type ft = F.t
