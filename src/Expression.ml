type expression =
  | Const of int
  | Var of string
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Pow of expression * int
  | Exp of expression

let rec print_expression e =
  match e with
  | Const c -> print_int c
  | Var x -> print_string x
  | Add (e1, e2) -> print_binop e1 "+" e2
  | Sub (e1, e2) -> print_binop e1 "-" e2
  | Mul (e1, e2) -> print_binop e1 "*" e2
  | Div (e1, e2) -> print_binop e1 "/" e2
  | Pow (e, n) ->
    print_string "(";
    print_expression e;
    print_string "^";
    print_int n;
    print_string ")"
  | Exp e -> print_func "exp" e
and print_binop e1 op e2 =
  print_string "(";
  print_expression e1;
  print_string op;
  print_expression e2;
  print_string ")"
and print_func f e =
  print_string f;
  print_string "(";
  print_expression e;
  print_string ")"

let rec pow_int x n =
  match n with
  | 0 -> 1
  | _ when n mod 2 = 0 ->
    let y = pow_int x (n / 2) in
    y * y
  | _ ->
    let y = pow_int x (n / 2) in
    y * y * x

let rec simplify e =
  match e with
  | Const c -> e
  | Var x -> e
  | Add (e1, e2) ->
    simplify_add (simplify e1) (simplify e2)
  | Sub (e1, e2) ->
    simplify_sub (simplify e1) (simplify e2)
  | Mul (e1, e2) ->
    simplify_mul (simplify e1) (simplify e2)
  | Div (e1, e2) ->
    simplify_div (simplify e1) (simplify e2)
  | Pow (u, n) ->
    simplify_pow (simplify u) n
  | _ -> e
and simplify_add u1 u2 =
  match (u1, u2) with
  | (Const 0, _) -> u2
  | (_, Const 0) -> u1
  | (Const x, Const y) -> Const (x + y)
  | _ -> Add (u1, u2)
and simplify_sub u1 u2 =
  match (u1, u2) with
  | (_, Const 0) -> u1
  | (Const x, Const y) -> Const (x - y)
  | _ -> Sub (u1, u2)
and simplify_mul u1 u2 =
  match (u1, u2) with
  | (Const 0, _) -> Const 0
  | (_, Const 0) -> Const 0
  | (Const 1, _) -> u2
  | (_, Const 1) -> u1
  | (Const x, Const y) -> Const (x * y)
  | _ -> Mul (u1, u2)
and simplify_div u1 u2 =
  match (u1, u2) with
  | (Const 0, _) -> Const 0
  | (_, Const 1) -> u1
  | (Const x, Const y) when x mod y = 0 -> Const (x / y)
  | _ -> Div (u1, u2)
and simplify_pow u n =
  match (u, n) with
  | (Const c, _) -> Const (pow_int c n)
  | (_, 0) -> Const 1
  | (_, 1) -> u
  | (Pow (v, m), _) -> Pow (v, m * n)
  | _ -> Pow (u, n)

let rec derivate e =
  simplify (derivate_impl e)
and derivate_impl e =
  match e with
  | Const c -> Const 0
  | Var x -> Const 1
  | Add (e1, e2) -> Add (derivate e1, derivate e2)
  | Sub (e1, e2) -> Sub (derivate e1, derivate e2)
  | Mul (e1, e2) -> Add (Mul (derivate e1, e2), Mul (e1, derivate e2))
  | Div (e1, e2) -> Div (Sub (Mul (derivate e1, e2), Mul (e1, derivate e2)), Pow (e2, 2))
  | Pow (u, n) -> Mul (derivate u, Pow (u, n - 1))
  | Exp u -> Mul (derivate u, e)
