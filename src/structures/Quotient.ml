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
  
  val make: rt -> t
end


module MakeQuotient (R : EuclidianRing) (V : Value with type t = R.t) = struct
  
  type rt = R.t
  type t = R.t
  
  
  let to_string (p : t) : string =
    R.to_string p
  
  
  let make (p : R.t) : t =
    R.rem p V.x
  
  
  let zero () : t =
    R.zero ()
  
  let one () : t =
    R.one ()
  
  let is_zero (p : t) : bool =
    R.is_zero p
  
  let is_one (p : t) : bool =
    R.is_one p
  
  
  let opposite (p : t) : t =
    R.rem (R.opposite p) V.x
  
  let inverse (p : t) : t =
    let q, _, g = R.bezout p V.x in
    let qq = R.mul q (R.unit_part g) in
    R.rem qq V.x
  
  let of_int (i : int) : t =
    R.rem (R.of_int i) V.x
  
  let mul_int (i : int) (p : t) : t =
    R.rem (R.mul p (R.of_int i)) V.x
  
  
  let add (p1 : t) (p2 : t) : t =
    R.rem (R.add p1 p2) V.x
  
  let sub (p1 : t) (p2 : t) : t =
    R.rem (R.sub p1 p2) V.x
  
  let mul (p1 : t) (p2 : t) : t =
    R.rem (R.mul p1 p2) V.x
  
  let div (p1 : t) (p2 : t) : t =
    let q2, _, g = R.bezout p2 V.x in
    let qq2 = R.mul q2 (R.unit_part g) in
    R.rem (R.mul p1 qq2) V.x
  
end
