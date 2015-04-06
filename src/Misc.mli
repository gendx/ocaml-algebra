module Misc : sig
  (* Args    : f, g, h, a, b *)
  (* Returns : c the list such that each element is : *)
  (*               (f a_i b_i) if i < min(len(a), len(b)) *)
  (*               (g a_i) if len(b) <= i < len(a) *)
  (*               (f b_i) if len(a) <= i < len(b) *)
  val map3: ('a -> 'b -> 'c) -> ('a -> 'c) -> ('b -> 'c) -> 'a list -> 'b list -> 'c list
  (* Args    : f, x, y *)
  (* Returns : the lexicographical comparison of x and y *)
  (*               according to the comparison function f *)
  val lexcompare: ('a -> 'a -> int) -> ('a list) -> ('a list) -> int
  (* Args    : x *)
  (* Returns : the list of pairs {y, z} with y, z are distinct elements of x *)
  val pairs: ('a list) -> (('a * 'a) list)
  (* Args    : s, x *)
  (* Returns : a printable string of the monomial x with variable name s *)
  val mono_to_string: string -> int -> string
end

module type Value = sig
  type t
  val x: t
end
