open Polynomial
open Ring
open Field
open Misc
open GCD

module type SparsePolynomial = sig
  type ft
  include Polynomial with type ft := ft and type t = (ft * int) list
end

module type EuclidianSparsePolynomial = sig
  type ft
  include EuclidianPolynomial with type ft := ft and type t = (ft * int) list
end


module MakeSparsePolynomial (R : Ring) = struct
  
  type ft = R.t
  type t = (R.t * int) list
  
  
  let print_var (name : string) (x : t) : unit =
    print_string "(";
    
    let started = List.fold_left
      (fun started (a, n) ->
          if not (R.is_zero a) then (
            if started then
              print_string " + ";
            if n = 0 || not (R.is_one a) then
              R.print a;
            Misc.print_mono name n;
            true
          ) else
            started
      )
    false x in
    
    if not started then
      R.print (R.zero ());
    
    print_string ")"
  
  let print (x : t) : unit =
    print_var "x" x
  
  
  let clean (x : t) : t =
    List.fold_right
      (fun (a, n) l ->
          if R.is_zero a then
            l
          else
            (a, n)::l
      )
    x []
  
  let make (x : R.t list) : t =
    let l, _ = List.fold_left
      (fun (l, n) a ->
          let ll = if R.is_zero a then l else (a, n)::l in
          (ll, n + 1)
      )
    ([], 0) x
    in l
  
  let make_of (f : 'a -> R.t) (x : 'a list) : t =
    make (List.map f x)
  
  
  let make_monom (a : R.t) (n : int) : t =
    if R.is_zero a then
      []
    else
      [a, n]
  
  let x () : t =
    [R.one (), 1]
  
  
  let zero () : t =
    []
  
  let one () : t =
    [R.one (), 0]
  
  let is_zero (x : t) : bool =
    x = []
  
  let is_one (x : t) : bool =
    match x with
    | [a, n] when n = 0 && (R.is_one a) -> true
    | _ -> false
  
  
  let opposite (x : t) : t =
    List.map (fun (a, n) -> (R.opposite a, n)) x
  
  let of_int (x : int) : t =
    let y = R.of_int x in
    if R.is_zero y then
      []
    else
      [y, 0]
  
  let mul_int (i : int) (x : t) : t =
    clean (List.map (fun (a, n) -> (R.mul_int i a, n)) x)
  
  let mul_field (lambda : R.t) (x : t) : t =
    List.map (fun (a, n) -> (R.mul lambda a, n)) x
  
  
  let add (x : t) (y : t) : t =
    let rec add_impl x y =
      match (x, y) with
      | ([], _) -> y
      | (_, []) -> x
      | ((a, n)::q, (b, m)::r) ->
        if n > m then
          (a, n)::(add_impl q y)
        else if n < m then
          (b, m)::(add_impl x r)
        else
          (R.add a b, n)::(add_impl q r)
    in
    clean (add_impl x y)
  
  let sub (x : t) (y : t) : t =
    let rec sub_impl x y =
      match (x, y) with
      | ([], _) -> opposite y
      | (_, []) -> x
      | ((a, n)::q, (b, m)::r) ->
        if n > m then
          (a, n)::(sub_impl q y)
        else if n < m then
          (R.opposite b, m)::(sub_impl x r)
        else
          (R.sub a b, n)::(sub_impl q r)
    in
    clean (sub_impl x y)
  
  let mul_monom ((lambda, k) : R.t * int) (x : t) : t =
    List.map (fun (a, n) -> (R.mul lambda a, n + k)) x
  
  let mul (x : t) (y : t) : t =
    clean (List.fold_left
            (fun p a -> add (mul_monom a y) p)
          [] x)
  
  
  let derivate (deriv : t) (deriv_const : (R.t -> R.t)) (x : t) : t =
    List.fold_left
      (fun p (a, n) ->
          let b = deriv_const a in
          let q = if R.is_zero b then p else add p [b, n] in
          
          if n > 0 then (
            let monom = (mul deriv [R.mul_int n a, n - 1]) in
            add q monom
          ) else
            q
      )
    [] x
  
end


module MakeEuclidianSparsePolynomial (F : Field) = struct
  
  module rec Implementation :
    EuclidianSparsePolynomial with type ft = F.t
  = struct
    
    include MakeSparsePolynomial(F)
    
    module GCDImpl = GCD(Implementation)
    
    
    let unit_part_impl (x : t) : F.t =
      match x with
      | [] -> F.one ()
      | (a, _)::q -> F.inverse a
    
    let unit_part (x : t) : t =
      [unit_part_impl x, 0]
    
    let normalize (x : t) : (t * t) =
      let k = unit_part_impl x in
      (mul_field k x, [k, 0])
    
    let leading (x : t) : F.t * int =
      match x with
      | [] -> (F.zero (), -1)
      | t::_ -> t
    
    
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
      (snd (leading x)) < (snd (leading y))
    
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
