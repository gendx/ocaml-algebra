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

open Misc

module MultiPower = struct
  
  type t = ((int * int) list) * int
  
  
  let to_string (x : t) : string =
    let s, _ = List.fold_left
      (fun (s, started) (a, b) ->
          s ^ (if started then ", " else "") ^
          "(" ^ (string_of_int a) ^ ", " ^ (string_of_int b) ^ ")",
          true
      )
    ("", false) (fst x) in
    "{" ^ s ^ "}"
  
  let power_to_string (names : int -> string) (x : t) : string =
    List.fold_left (fun s (var, k) -> s ^ (Misc.mono_to_string (names var) k)) "" (fst x)
  
  
  let make (x : (int * int) list) : t =
    (
      List.sort (fun (a, n) (b, m) -> a - b) x,
      List.fold_left (fun total (_, k) -> total + k) 0 x
    )
    
  
  let zero () : t =
    ([], 0)
  
  let is_zero (n : t) : bool =
    snd n = 0
  
  
  let compare_lex ((n1, s1) : t) ((n2, s2) : t) : int =
    Misc.lexcompare
      (fun (x, a) (y, b) ->
          if x = y then
            a - b
          else
            y - x
      )
    n1 n2
  
  let compare_grlex ((n1, s1) : t) ((n2, s2) : t) : int =
    if s1 = s2 then
      compare_lex (n1, s1) (n2, s2)
    else
      s1 - s2
  
  let compare (x : t) (y : t) : int =
    compare_lex x y
  
  let add (x : t) (y : t) : t =
    let rec add_impl (n1, s1) (n2, s2) =
      match (n1, n2) with
      | ([], _) -> (n2, s2)
      | (_, []) -> (n1, s1)
      | ((a, n)::q, (b, m)::r) ->
        if a < b then (
          let x, s = add_impl (q, s1 - n) (n2, s2) in
          (a, n)::x, s + n
        ) else if a > b then (
          let x, s = add_impl (n1, s1) (r, s2 - m) in
          (b, m)::x, s + m
        ) else (
          let x, s = add_impl (q, s1 - n) (r, s2 - m) in
          if n + m > 0 then (
            (a, n + m)::x, s + n + m
          ) else
            x, s
        )
    in
    add_impl x y
    
  let rec add_var ((a, k) : int * int) ((n, s) : t) : t =
    match n with
    | [] -> [a, k], k
    | (b, l)::q when a = b ->
      if l + k > 0 then
        (a, l + k)::q, s + k
      else
        q, s - l
    | (b, _)::q when a < b ->
      (a, k)::n, s + k
    | (b, l)::q ->
      let x, s = add_var (a, k) (q, s - l) in
      (b, l)::x, s
  
  
  let rec gcd ((n1, s1) : t) ((n2, s2) : t) : t =
    match (n1, n2) with
    | ([], _)
    | (_, []) -> [], 0
    | ((a, n)::q, (b, m)::r) ->
      if a < b then
        gcd (q, s1 - n) (n2, s2)
      else if a > b then
        gcd (n1, s1) (r, s2 - m)
      else (
        let x, s = gcd (q, s1 - n) (r, s2 - m) in
        let p = if n < m then n else m in
        (a, p)::x, s + p
      )
  
  let rec gcd_reduction ((n1, s1) : t) ((n2, s2) : t) : (t * t) =
    match (n1, n2) with
    | ([], _)
    | (_, []) -> ((n1, s1), (n2, s2))
    | ((a, n)::q, (b, m)::r) ->
      if a < b then (
        let (m1, s), m2 = gcd_reduction (q, s1 - n) (n2, s2) in
        ((a, n)::m1, s + n), m2
      ) else if a > b then (
        let m1, (m2, s) = gcd_reduction (n1, s1) (r, s2 - m) in
        m1, ((b, m)::m2, s + m)
      ) else (
        let (m1, ss1), (m2, ss2) = gcd_reduction (q, s1 - n) (r, s2 - m) in
        
        if n < m then
          (m1, ss1), ((b, m - n)::m2, ss2 + m - n)
        else if n > m then
          ((a, n - m)::m1, ss1 + n - m), (m2, ss2)
        else
          (m1, ss1), (m2, ss2)
      )
  
end
