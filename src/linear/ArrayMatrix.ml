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
open Vector
open Pervasives

module type ArrayMatrix = sig
  type ft
  include SquaredMatrix with type ft := ft and type t = ft array array
  
  val of_vects: Vector.t -> Vector.t -> t
end


module MakeArrayMatrix (R : Ring) (Size : IntValue) = struct
  
  type ft = R.t
  type t = R.t array array
  
  module Vector = MakeVector(R)(Size)
  
  
  let at (x : t) (i : int) (j : int) : R.t =
    Array.get (Array.get x i) j
  
  let set (x : t) (i : int) (j : int) (v : R.t) =
    Array.set (Array.get x i) j v
  
  let to_string (x : t) : string =
    let s = ref "" in
    for i = 0 to Size.x - 1 do
      s := !s ^
      (if i = 0 then "[" else " ");
      
      for j = 0 to Size.x - 1 do
        s := !s ^
        (if j > 0 then " " else "") ^
        (R.to_string (at x i j))
      done;
      
      s := !s ^
      (if i = Size.x - 1 then "]" else "\n")
    done;
    !s
  
  
  let of_vects (x : Vector.t) (y : Vector.t) : t =
    let m = Array.make_matrix Size.x Size.x (R.zero ()) in
    for i = 0 to Size.x - 1 do
      for j = 0 to Size.x - 1 do
        set m i j (R.mul (Array.get x i) (Array.get y j))
      done;
    done;
    m
    
    
  let zero () : t =
    Array.make_matrix Size.x Size.x (R.zero ())
  
  let one () : t =
    let m = Array.make_matrix Size.x Size.x (R.zero ()) in
    for i = 0 to Size.x - 1 do
      set m i i (R.one ())
    done;
    m
  
  let is_zero (x : t) : bool =
    Array.fold_left
      (fun result y ->
          result &&
            (
              Array.fold_left (fun result z -> result && (R.is_zero z)) true y
            )
      )
    true x
  
  let is_one (x : t) : bool =
    try
      for i = 0 to Size.x - 1 do
        for j = 0 to Size.x - 1 do
          let y = at x i j in
          if not ((i = j && R.is_one y) || (R.is_zero y)) then
            raise Exit;
        done;
      done;
      true
    with Exit -> false
    
    
  let opposite (x : t) : t =
    Array.map (Array.map R.opposite) x
    
  let of_int (i : int) : t =
    let m = Array.make_matrix Size.x Size.x (R.zero ()) in
    for i = 0 to Size.x - 1 do
      set m i i (R.of_int i)
    done;
    m
  
  let mul_int (i : int) (x : t) : t =
    Array.map (Array.map (R.mul_int i)) x
  
  let mul_field (lambda : R.t) (x : t) : t =
    Array.map (Array.map (R.mul lambda)) x
    
  
  let add (x : t) (y : t) : t =
    Array.mapi
      (fun i u ->
          Array.mapi (fun j z -> R.add z (at y i j)) u
      )
    x
  
  let sub (x : t) (y : t) : t =
    Array.mapi
      (fun i u ->
          Array.mapi (fun j z -> R.sub z (at y i j)) u
      )
    x
  
  let mul (x : t) (y : t) : t =
    Array.mapi
      (fun i u ->
          Array.mapi (fun j z -> R.mul z (at y i j)) u
      )
    x
    
    
  let mul_vect (x : t) (v : Vector.t) : Vector.t =
    Array.mapi
      (fun i u ->
          let result, _ = Array.fold_left
            (fun (result, j) z ->
                (R.add result (R.mul z (Array.get v j)), j + 1)
            )
          (R.zero (), 0) u
          in result
      )
    x
  
end
