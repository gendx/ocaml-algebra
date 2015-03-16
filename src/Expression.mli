type expression =
  | Const of int
  | Var of string
  | Add of expression * expression
  | Sub of expression * expression
  | Mul of expression * expression
  | Div of expression * expression
  | Pow of expression * int
  | Exp of expression

val print_expression : expression -> unit

val derivate : expression -> expression

val simplify : expression -> expression
