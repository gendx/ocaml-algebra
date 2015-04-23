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
open Field
open Misc

module type Quotient = sig
  type rt
  include Field with type t = rt
  
  (* Args    : x *)
  (* Returns : y, the canonical representation of x in the quotient field *)
  val make: rt -> t
end

module MakeQuotient (R : EuclidianRing) (V : Value with type t = R.t) :
  Quotient with type rt = R.t
