module Misc = struct
  
  let rec map3 fun1 fun2 fun3 x y =
    match x, y with
    | [], [] -> []
    | [], b::r -> (fun3 b)::(map3 fun1 fun2 fun3 [] r)
    | a::q, [] -> (fun2 a)::(map3 fun1 fun2 fun3 q [])
    | a::q, b::r -> (fun1 a b)::(map3 fun1 fun2 fun3 q r)
    
  let rec lexcompare comp x y =
    match x, y with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | a::q, b::r ->
      let c = comp a b in
      if c = 0 then
        lexcompare comp q r
      else
        c
  
  let rec pairs x =
    match x with
    | [] -> []
    | t::q ->
      List.fold_right (fun r l -> (t, r)::l) q (pairs q)
      (*List.fold_left (fun l r -> (t, r)::l) (pairs q) q*)
  
  let mono_to_string name n =
    match n with
    | 0 -> ""
    | 1 -> name
    | n -> name ^ "^" ^ (string_of_int n)

end

module type Value = sig
  type t
  val x: t
end
