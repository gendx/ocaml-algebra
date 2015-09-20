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

open MultiPower
open Algebra
open Field
open Misc

module type MultivarPolynomial = sig
  type ft
  include Algebra with type ft := ft and type t = (ft * MultiPower.t) list
  
  val to_string_with: (int -> string) -> t -> string
  val to_string_names: string array -> t -> string
  
  val make: (ft * ((int * int) list)) list -> t
  val make_of: ('a -> ft) -> ('a * ((int * int) list)) list -> t
  
  val derivate: t array -> t -> t
  
  val normalize: t -> t
  val spoly: t -> t -> t
  val division: t -> t -> (t * t)
  val division_set: t -> t array -> (t array * t)
  val groebner: t list -> t list
end


module MakeMultivarPolynomial (F : Field) = struct
  
  type ft = F.t
  type t = (ft * MultiPower.t) list
  
  
  let rec generate_name (i : int) : string =
    let nth_letter i =
      if i < 3 then
        Char.chr ((Char.code 'x') + i)
      else if i < 7 then
        Char.chr ((Char.code 't') + i - 3)
      else
        Char.chr ((Char.code 'a') + i - 7)
    in
    
    let rec impl i =
      let s = String.make 1 (nth_letter (i mod 26)) in
      if i < 26 then
        [s]
      else
        s::(impl (i / 26))
    in
    
    String.concat "" (impl i)
  
  let to_string_with (names : int -> string) (x : t) : string =
    let s, started = List.fold_left
      (fun (s, started) (a, n) ->
          if not (F.is_zero a) then (
            let ss =
              (if started then " + " else "") ^
              (if MultiPower.is_zero n || not (F.is_one a) then F.to_string a else "") ^
              (MultiPower.power_to_string names n)
            in
            (s ^ ss, true)
          ) else
            (s, started)
      )
    ("", false) x in
    
    "(" ^ (if started then s else F.to_string (F.zero ())) ^ ")"
  
  let to_string (x : t) : string =
    to_string_with generate_name x
  
  let to_string_names (names : string array) (x : t) : string =
    to_string_with (Array.get names) x
  
  
  let clean (x : t) : t =
    List.fold_right
      (fun (a, n) l ->
          if F.is_zero a then
            l
          else
            (a, n)::l
      )
    x []
  
  let make (x : (F.t * ((int * int) list)) list) : t =
    List.sort
      (fun (a, n) (b, m) -> MultiPower.compare m n)
    (clean (List.map (fun (a, n) -> (a, MultiPower.make n)) x))
  
  let make_of (f : 'a -> F.t) (x : ('a * ((int * int) list)) list) : t =
    make (List.map (fun (a, n) -> (f a, n)) x)
  
  
  let zero () : t =
    []
  
  let one () : t =
    [F.one (), MultiPower.zero ()]
  
  let is_zero (x : t) : bool =
    x = []
  
  let is_one (x : t) : bool =
    match x with
    | [a, n] when MultiPower.is_zero n && (F.is_one a) -> true
    | _ -> false
  
  
  let opposite (x : t) : t =
    List.map (fun (a, n) -> (F.opposite a, n)) x
  
  let of_int (x : int) : t =
    let y = F.of_int x in
    if F.is_zero y then
      []
    else
      [y, MultiPower.zero ()]
  
  let mul_int (i : int) (x : t) : t =
    clean (List.map (fun (a, n) -> (F.mul_int i a, n)) x)
  
  let mul_field (lambda : F.t) (x : t) : t =
    clean (List.map (fun (a, n) -> (F.mul lambda a, n)) x)
  
  
  let add (x : t) (y : t) : t =
    let rec add_impl x y =
      match (x, y) with
      | ([], _) -> y
      | (_, []) -> x
      | ((a, n)::q, (b, m)::r) ->
        let cmp = MultiPower.compare n m in
        
        if cmp > 0 then
          (a, n)::(add_impl q y)
        else if cmp < 0 then
          (b, m)::(add_impl x r)
        else
          (F.add a b, n)::(add_impl q r)
    in
    clean (add_impl x y)
  
  let sub (x : t) (y : t) : t =
    let rec sub_impl x y =
      match (x, y) with
      | ([], _) -> opposite y
      | (_, []) -> x
      | ((a, n)::q, (b, m)::r) ->
        let cmp = MultiPower.compare n m in
        
        if cmp > 0 then
          (a, n)::(sub_impl q y)
        else if cmp < 0 then
          (F.opposite b, m)::(sub_impl x r)
        else
          (F.sub a b, n)::(sub_impl q r)
    in
    clean (sub_impl x y)
  
  let mul_monom ((lambda, k) : F.t * MultiPower.t) (x : t) : t =
    List.map (fun (a, n) -> (F.mul lambda a, MultiPower.add n k)) x
  
  let mul (x : t) (y : t) : t =
    clean (List.fold_left
      (fun p a -> add (mul_monom a y) p)
    [] x)
  
  
  let derivate (deriv : t array) (x : t) : t =
    List.fold_left
      (fun p (a, n) ->
          List.fold_left
            (fun q (var, k) ->
                let monom = [F.mul_int k a, MultiPower.add_var (var, -1) n] in
                add q (mul (Array.get deriv var) monom)
            )
          p (fst n)
      )
    [] x
  
  
  let leading (x : t) : F.t * MultiPower.t =
    match x with
    | [] -> (F.zero (), MultiPower.zero ())
    | t::_ -> t
  
  let normalize (x : t) : t =
    match x with
    | [] -> []
    | (a, _)::_ -> mul_field (F.inverse a) x
  
  let spoly (x : t) (y : t) : t =
    if is_zero x then
      opposite y
    else if is_zero y then
      x
    else (
      let (lx, nx) = leading x in
      let (ly, ny) = leading y in
      let nxx, nyy = MultiPower.gcd_reduction nx ny in
      
      sub (mul_monom (F.inverse lx, nyy) x) (mul_monom (F.inverse ly, nxx) y)
    )
  
  let division (x : t) (y : t) : (t * t) =
    let rec division_impl x y (ly, ny) =
      if is_zero x then
        (zero (), zero ())
      else (
        let (lx, nx) = leading x in
        let check, m = MultiPower.gcd_reduction ny nx in
        
        (* nx = m * ny *)
        if MultiPower.is_zero check then (
          let monom = (F.div lx ly, m) in
          let q, r = division_impl (sub x (mul_monom monom y)) y (ly, ny) in
          
          (add q [monom], r)
        ) else (
          let q, r = division_impl (sub x [lx, nx]) y (ly, ny) in
          
          (q, add r [lx, nx])
        )
      )
    in
    
    division_impl x y (leading y)
  
  let division_set (x : t) (f : t array) : (t array * t) =
    let n = Array.length f in
    let q = Array.make n [] in
    let r = ref [] in
    let p = ref x in
    
    while not (is_zero !p) do
      let (lp, np) = leading !p in
      
      let i = ref 0 in
      let search_dividing = ref true in
      
      while !i < n && !search_dividing do
        let fi = Array.get f !i in
        let (li, ni) = leading fi in
        
        let check, m = MultiPower.gcd_reduction ni np in
        
        if MultiPower.is_zero check then (
          let qi = Array.get q !i in
          let monom = (F.div lp li, m) in
          
          Array.set q !i (add qi [monom]);
          p := sub !p (mul_monom monom fi);
          search_dividing := false;
        ) else
          incr i;
      done;
      
      if !search_dividing then (
        r := add !r [lp, np];
        p := sub !p [lp, np];
      )
    done;
    
    (q, !r)
  
  
  let reduce (x : t list) : t list =
    let rec reduce_impl_core x reduced =
      match x with
      | [] -> reduced, false
      | t::q ->
        let b = reduced@q in
        
        let _, rr = division_set t (Array.of_list b) in
        let r = normalize rr in
        
        if not (is_zero (sub t r)) then (
          print_string "reduction : ";
          print_string (to_string t);
          print_string " -> ";
          print_string (to_string r);
          print_newline ();
        );
        
        if is_zero r then
          (b, true)
        else if is_zero (sub t r) then
          reduce_impl_core q (r::reduced)
        else
          (r::b, true)
        (*
        if is_zero r then
          reduce_impl_core q reduced true
        else (
          let m = modified || (not (is_zero (sub t r))) in
          reduce_impl_core q (r::reduced) m
        )
        *)
    in
    
    let rec reduce_impl x =
      let reduced, modified = reduce_impl_core x [] in
      if modified then
        reduce_impl reduced
      else
        reduced
    in
    
    reduce_impl x
  
  let groebner (x : t list) : (t list) =
    let rec groebner_impl polys pairs =
      match pairs with
      | [] -> polys
      | (a, b)::q ->
        let _, na = leading a in
        let _, nb = leading b in
        
        if MultiPower.is_zero (MultiPower.gcd na nb) then (
          print_string "relatively prime : ";
          print_string (to_string a);
          print_string " & ";
          print_string (to_string b);
          print_newline ();
          
          groebner_impl polys q
        ) else (
          let s = spoly a b in
          let _, rr = division_set s (Array.of_list polys) in
          let r = normalize rr in
          
          print_string (to_string a);
          print_string " & ";
          print_string (to_string b);
          print_string " = ";
          print_string (to_string s);
          print_string " -> ";
          print_string (to_string r);
          print_newline ();
          
          if is_zero r then
            groebner_impl polys q
          else (
            let reduced = reduce (r::polys) in
            print_endline "reduction of {";
            List.iter (fun t ->  print_string "\t"; print_string (to_string t); print_newline ()) (r::polys);
            print_endline "} = {";
            List.iter (fun t ->  print_string "\t"; print_string (to_string t); print_newline ()) reduced;
            print_string "}";
            print_newline ();
            
            (*
            let ppairs = List.fold_left (fun l t -> (r, t)::l) pairs polys in
            groebner_impl (r::polys) ppairs
            *)
            groebner_impl reduced (Misc.pairs reduced)
          )
        )
    in
    
    let xx = List.map normalize x in
    groebner_impl xx (Misc.pairs xx)
  
end
