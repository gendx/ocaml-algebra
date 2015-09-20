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
open Errors

module type DiagonalMatrixFunctions = sig
  type ft
  type t

  val make: ft array -> t
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


module MakeDiagonalMatrix (R : Ring) (Size : IntValue) = struct
  
  type ft = R.t
  type t = R.t array
  
  module Vector = MakeVector(R)(Size)
  
  
  let to_string (x : t) : string =
    let s = ref "[" in
    let rzero = R.zero () in

    for i = 0 to Size.x - 1 do
      s := !s ^ "[";
      
      for j = 0 to Size.x - 1 do
        s := !s ^
          (if j > 0 then " " else "") ^
          (R.to_string (if i = j then x.(i) else rzero))
      done;
      
      s := !s ^ "]" ^
        (if i = Size.x - 1 then "]" else " ")
    done;
    !s
    
    
  let check_dim (x : 'a array) : bool =
    Array.length x = Size.x

  let make (x : R.t array) : t =
    if not (check_dim x) then
      raise Errors.Invalid_dimension;
    x

  let make_of (f : 'a -> R.t) (x : 'a array) : t =
    if not (check_dim x) then
      raise Errors.Invalid_dimension;
    Array.map f x


  let zero () : t =
    Array.make Size.x (R.zero ())
  
  let one () : t =
    Array.make Size.x (R.one ())
  
  let is_zero (x : t) : bool =
    Array.fold_left (fun result y -> result && (R.is_zero y)) true x
  
  let is_one (x : t) : bool =
    Array.fold_left (fun result y -> result && (R.is_one y)) true x
    
    
  let opposite (x : t) : t =
    Array.map R.opposite x
    
  let of_int (i : int) : t =
    Array.make Size.x (R.of_int i)
  
  let mul_int (i : int) (x : t) : t =
    Array.map (R.mul_int i) x
  
  let mul_field (lambda : R.t) (x : t) : t =
    Array.map (R.mul lambda) x
    
  
  let add (x : t) (y : t) : t =
    Array.mapi (fun i z -> R.add z y.(i)) x
  
  let sub (x : t) (y : t) : t =
    Array.mapi (fun i z -> R.sub z y.(i)) x
  
  let mul (x : t) (y : t) : t =
    Array.mapi (fun i z -> R.mul z y.(i)) x
    
    
  let transpose (x : t) : t =
    x

  let trace (x : t) : R.t =
    Array.fold_left (fun result y -> R.add result y) (R.zero ()) x

  let mul_vect (x : t) (v : Vector.t) : Vector.t =
    Array.mapi (fun i z -> R.mul z v.(i)) x
  
end


module MakeDiagonalFieldMatrix (F : Field) (Size : IntValue) = struct

  include MakeDiagonalMatrix(F)(Size)


  let determinant (x : t) : F.t =
    Array.fold_left (fun result y -> F.mul result y) (F.one ()) x

  let inverse (x : t) : t =
    Array.map F.inverse x
 
end
