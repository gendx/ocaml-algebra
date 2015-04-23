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
open Misc
open GCD

module type ListPolynomial = sig
  type ft
  include Polynomial with type ft := ft and type t = ft list
end

module type EuclidianListPolynomial = sig
  type ft
  include EuclidianPolynomial with type ft := ft and type t = ft list
end


module MakeListPolynomial (R : Ring) = struct
  
  type ft = R.t
  type t = R.t list
  
  
  let to_string_var (name : string) (x : t) : string =
    let s, started, _ = List.fold_left
      (fun (s, started, n) a ->
          if not (R.is_zero a) then (
            let ss =
              (if started then " + " else "") ^
              (if n = 0 || not (R.is_one a) then R.to_string a else "") ^
              (Misc.mono_to_string name n)
            in
            (s ^ ss, true, n + 1)
          ) else
            (s, started, n + 1)
      )
    ("", false, 0) x in
    
    "(" ^ (if started then s else R.to_string (R.zero ())) ^ ")"
  
  let to_string (x : t) : string =
    to_string_var "x" x
  
  
  let clean (x : R.t list) : t =
    List.fold_right
      (fun a p ->
          match p with
          | [] when R.is_zero a -> []
          | _ -> a::p
      )
    x []
  
  let make (x : R.t list) : t =
    clean x
  
  let make_of (f : 'a -> R.t) (x : 'a list) : t =
    clean (List.map f x)
  
  
  let rec shift (x : t) (k : int) : t =
    match x with
    | [] -> []
    | _ when k = 0 -> x
    | _ -> (R.zero ())::(shift x (k - 1))
  
  let make_monom (a : R.t) (n : int) : t =
    if R.is_zero a then
      []
    else
      shift [a] n
  
  let x () : t =
    [R.zero () ; R.one ()]
  
  
  let zero () : t =
    []
  
  let one () : t =
    [R.one ()]
  
  let is_zero (x : t) : bool =
    x = []
  
  let is_one (x : t) : bool =
    match x with
    | [a] when R.is_one a -> true
    | _ -> false
  
  
  let opposite (x : t) : t =
    List.map R.opposite x
  
  let of_int (x : int) : t =
    let y = R.of_int x in
    if R.is_zero y then
      []
    else
      [y]
  
  let mul_int (i : int) (x : t) : t =
    clean (List.map (R.mul_int i) x)
  
  let mul_field_impl (lambda : R.t) (x : t) : t =
    List.map (R.mul lambda) x
  
  let mul_field (lambda : R.t) (x : t) : t =
    clean (mul_field_impl lambda x)
  
  
  let add (x : t) (y : t) : t =
    clean (Misc.map3 R.add (fun x -> x) (fun x -> x) x y)
  
  let sub (x : t) (y : t) : t =
    clean	(Misc.map3 R.sub (fun x -> x) R.opposite x y)
  
  let shift_one (x : t) : t =
    match x with
    | [] -> []
    | _ -> (R.zero ())::x
  
  let mul (x : t) (y : t) : t =
    clean (List.fold_right
            (fun a p -> add (mul_field_impl a y) (shift_one p))
          x [])
  
  
  let derivate (deriv : t) (deriv_const : (R.t -> R.t)) (x : t) : t =
    let p, _ = List.fold_left
      (fun (p, n) a ->
          let b = deriv_const a in
          let q = if R.is_zero b then p else add p (make_monom b n) in
          
          let r =
            if n > 0 then (
              let monom = make_monom (R.mul_int n a) (n - 1) in
              let monom2 = mul deriv monom in
              add q monom2
            ) else
              q
          in
          
          (r, n + 1)
      )
    ([], 0) x
    in p
  
end


module MakeEuclidianListPolynomial (F : Field) = struct
  
  module rec Implementation :
    EuclidianListPolynomial with type ft = F.t
  = struct
    
    include MakeListPolynomial(F)
    
    module GCDImpl = GCD(Implementation)
    
    
    let rec unit_part_impl (x : t) : F.t =
      match x with
      | [] -> F.one ()
      | [a] -> F.inverse a
      | t::q -> unit_part_impl q
    
    let unit_part (x : t) : t =
      [unit_part_impl x]
    
    let normalize (x : t) : (t * t) =
      let y, k = (List.fold_right
        (fun a (p, k) ->
            match p with
            | [] ->
              ([F.one ()], F.inverse a)
            | _ ->
              ((F.mul a k)::p, k)
        )
      x ([], F.zero ()))
      in
      (y, [k])
    
    let rec leading (x : t) : F.t * int =
      match x with
      | [] -> (F.zero (), -1)
      | [a] -> (a, 0)
      | t::q ->
        let (a, n) = leading q in
        (a, n + 1)
    
    
    let rec quorem (x : t) (y : t) : (t * t) =
      let (lx, lenx) = leading x in
      let (ly, leny) = leading y in
      if lenx < leny then
        (zero (), x)
      else (
        let q = make_monom (F.div lx ly) (lenx - leny) in
        let r = sub x (mul y q) in
        let qq, rr = quorem r y in
        (add q qq, rr)
      )
    
    let quo (x : t) (y : t) : t =
      let q, _ = quorem x y in q
    
    let rem (x : t) (y : t) : t =
      let _, r = quorem x y in r
    
    let comp (x : t) (y : t) : bool =
      (List.length x) < (List.length y)
    
    let gcd (x : t) (y : t) : t =
      GCDImpl.euclid comp x y
    
    let gcd_reduction (x : t) (y : t) : (t * t) =
      let g, _ = normalize (gcd x y) in
      (quo x g, quo y g)
    
    let bezout (x : t) (y : t) : (t * t * t) =
      GCDImpl.bezout comp x y
    
  end
  
  include Implementation
  
end
