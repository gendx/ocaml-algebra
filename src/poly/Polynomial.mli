open Algebra

module type PolynomialFunctions = sig
  type ft
  type t
  
  (* Args    : s, x *)
  (* Returns : a printable string of x with variable name s *)
  val to_string_var: string -> t -> string
  
  (* Args    : x *)
  (* Returns : p, the polynomial whose coefficients are the elements of x *)
  (*               given in increasing order of powers *)
  val make: ft list -> t
  (* Args    : f, x *)
  (* Returns : p, the polynomial whose coefficients are the elements of x mapped by f *)
  (*               given in increasing order of powers *)
  val make_of: ('a -> ft) -> 'a list -> t
  (* Args    : x, n *)
  (* Returns : p, the monomial of coefficient x and power n *)
  val make_monom: ft -> int -> t
  
  (* Returns : x, the unknown *)
  (*               i.e. the element of coefficient one and power 1 *)
  val x: unit -> t
  
  (* Args    : y, f, p *)
  (* Returns : q, the derivative of p such that : *)
  (*               y is the derivative of the unknown *)
  (*               f is the derivation function of the coefficients *)
  val derivate: t -> (ft -> ft) -> t -> t
end

module type Polynomial = sig
  include Algebra
  include PolynomialFunctions with type ft := ft and type t := t
end

module type EuclidianPolynomial = sig
  include EuclidianAlgebra
  include PolynomialFunctions with type ft := ft and type t := t
end
