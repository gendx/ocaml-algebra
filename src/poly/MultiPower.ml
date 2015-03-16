open Misc

module MultiPower = struct
  
  type t = ((int * int) list) * int
  
  
  let print (x : t) : unit =
    print_string "{";
    let _ = List.fold_left
      (fun started (a, b) ->
          if started then
            print_string ", ";
          print_string "(";
          print_int a;
          print_string ", ";
          print_int b;
          print_string ")";
          true
      )
    false (fst x) in
    print_string "}"
  
  let print_power (names : int -> string) (x : t) : unit =
    List.iter (fun (var, k) -> Misc.print_mono (names var) k) (fst x)
  
  
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
