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

module type ArrayMatrixFunctions = sig
  type ft
  type t
  type vect_t
  
  val make: ft array array -> t
  val make_of: ('a -> ft) -> 'a array array -> t
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


module MakeArrayMatrix (R : Ring) (Size : IntValue) = struct
  
  type ft = R.t
  type t = R.t array array
  
  module Vector = MakeVector(R)(Size)
  
  
  let to_string (x : t) : string =
    let s = ref "[" in
    for i = 0 to Size.x - 1 do
      s := !s ^ "[";
      
      for j = 0 to Size.x - 1 do
        s := !s ^
          (if j > 0 then " " else "") ^
          (R.to_string x.(i).(j))
      done;
      
      s := !s ^ "]" ^
        (if i = Size.x - 1 then "]" else " ")
    done;
    !s
  
  
  let check_dim (x : 'a array array) : bool =
    if Array.length x != Size.x then
      false
    else (
      Array.fold_left
        (fun result y ->
            result && (Array.length y == Size.x)
        )
      true x
    )

  let make (x : R.t array array) : t =
    if not (check_dim x) then
      raise Errors.Invalid_dimension;
    x

  let make_of (f : 'a -> R.t) (x : 'a array array) : t =
    if not (check_dim x) then
      raise Errors.Invalid_dimension;
    Array.map (Array.map f) x

  let of_vects (x : Vector.t) (y : Vector.t) : t =
    let m = Array.make_matrix Size.x Size.x (R.zero ()) in
    for i = 0 to Size.x - 1 do
      for j = 0 to Size.x - 1 do
        m.(i).(j) <- (R.mul x.(i) y.(j))
      done;
    done;
    m
    
    
  let zero () : t =
    Array.make_matrix Size.x Size.x (R.zero ())
  
  let one () : t =
    let m = Array.make_matrix Size.x Size.x (R.zero ()) in
    for i = 0 to Size.x - 1 do
      m.(i).(i) <- (R.one ())
    done;
    m
  
  let is_zero (x : t) : bool =
    Array.fold_left
      (fun result y ->
          result &&
          (Array.fold_left (fun result z -> result && (R.is_zero z)) true y)
      )
    true x
  
  let is_one (x : t) : bool =
    try
      for i = 0 to Size.x - 1 do
        for j = 0 to Size.x - 1 do
          let y = x.(i).(j) in
          if not ((i = j && R.is_one y) || (i <> j && R.is_zero y)) then
            raise Exit;
        done;
      done;
      true
    with Exit -> false
    
    
  let opposite (x : t) : t =
    Array.map (Array.map R.opposite) x
    
  let of_int (x : int) : t =
    let m = Array.make_matrix Size.x Size.x (R.zero ()) in
    for i = 0 to Size.x - 1 do
      m.(i).(i) <- (R.of_int x)
    done;
    m
  
  let mul_int (i : int) (x : t) : t =
    Array.map (Array.map (R.mul_int i)) x
  
  let mul_field (lambda : R.t) (x : t) : t =
    Array.map (Array.map (R.mul lambda)) x
    
  
  let add (x : t) (y : t) : t =
    Array.mapi
      (fun i u ->
          Array.mapi (fun j z -> R.add z y.(i).(j)) u
      )
    x
  
  let sub (x : t) (y : t) : t =
    Array.mapi
      (fun i u ->
          Array.mapi (fun j z -> R.sub z y.(i).(j)) u
      )
    x
  
  let mul (x : t) (y : t) : t =
    let m = Array.make_matrix Size.x Size.x (R.zero ()) in
    for i = 0 to Size.x - 1 do
      for j = 0 to Size.x - 1 do
        for k = 0 to Size.x - 1 do
          m.(i).(j) <- R.add m.(i).(j) (R.mul x.(i).(k) y.(k).(j))
        done;
      done;
    done;
    m
    
    
  let transpose (x : t) : t =
    let m = Array.make_matrix Size.x Size.x (R.zero ()) in
    for i = 0 to Size.x - 1 do
      for j = 0 to Size.x - 1 do
        m.(i).(j) <- x.(j).(i)
      done;
    done;
    m

  let trace (x : t) : R.t =
    let result = ref (R.zero ()) in
    for i = 0 to Size.x - 1 do
      result := R.add !result x.(i).(i)
    done;
    !result


  let mul_vect (x : t) (v : Vector.t) : Vector.t =
    Array.mapi
      (fun i u ->
          let result, _ = Array.fold_left
            (fun (result, j) z ->
                (R.add result (R.mul z v.(j)), j + 1)
            )
          (R.zero (), 0) u
          in result
      )
    x
  
end


module MakeArrayFieldMatrix (F : Field) (Size : IntValue) = struct

  include MakeArrayMatrix(F)(Size)


  let determinant (x : t) : F.t =
    let y = Array.map Array.copy x in
    let result = ref (F.one ()) in

    begin
      try
        (* for each row *)
        for i = 0 to Size.x - 1 do
          (* find pivot for row i *)
          let pivot = ref Size.x in
          begin
            try
              for j = i to Size.x - 1 do
                if not (F.is_zero y.(i).(j)) then (
                  pivot := j;
                  raise Exit
                )
              done;
            with Exit -> ()
          end;
          
          (* no pivot found => determinant is zero *)
          if !pivot = Size.x then (
            result := F.zero ();
            raise Exit
          );

          (* swap pivot column with column i *)
          if !pivot != i then (
            for k = i to Size.x - 1 do
              let tmp = y.(k).(i) in
              y.(k).(i) <- y.(k).(!pivot);
              y.(k).(!pivot) <- tmp
            done
          );

          (* triangularize *)
          for k = i + 1 to Size.x - 1 do
            let factor = F.div y.(k).(i) y.(i).(i) in
            for l = i + 1 to Size.x - 1 do
              y.(k).(l) <- F.sub y.(k).(l) (F.mul factor y.(i).(l))
            done;
          done;

          (* update result *)
          result := F.mul !result y.(i).(i);
        done
      with Exit -> ()
    end;

    !result

  let inverse (x : t) : t =
    let y = Array.map Array.copy x in
    let z = one () in

    (* for each row *)
    for i = 0 to Size.x - 1 do
      (* find pivot for row i *)
      let pivot = ref Size.x in
      begin
        try
          for j = i to Size.x - 1 do
            if not (F.is_zero y.(i).(j)) then (
              pivot := j;
              raise Exit
            )
          done;
        with Exit -> ()
      end;
      
      (* no pivot found => not inversible *)
      if !pivot = Size.x then
        raise Errors.Not_inversible;

      (* swap pivot column with column i *)
      if !pivot != i then (
        for k = i to Size.x - 1 do
          let tmp = y.(k).(i) in
          y.(k).(i) <- y.(k).(!pivot);
          y.(k).(!pivot) <- tmp
        done;

        for k = 0 to Size.x - 1 do
          let tmp = z.(k).(i) in
          z.(k).(i) <- z.(k).(!pivot);
          z.(k).(!pivot) <- tmp
        done;
      );

      (* diagonalize *)
      for k = 0 to Size.x - 1 do
        if k != i then (
          let factor = F.div y.(k).(i) y.(i).(i) in

          for l = i + 1 to Size.x - 1 do
            y.(k).(l) <- F.sub y.(k).(l) (F.mul factor y.(i).(l))
          done;

          (* update result *)
          for l = 0 to Size.x - 1 do
            z.(k).(l) <- F.sub z.(k).(l) (F.mul factor z.(i).(l))
          done;
        )
      done;

      (* normalize *)
      let factor = y.(i).(i) in

      for l = i + 1 to Size.x - 1 do
        y.(i).(l) <- F.div y.(i).(l) factor
      done;

      for l = 0 to Size.x - 1 do
        z.(i).(l) <- F.div z.(i).(l) factor
      done;
    done;

    z

end
