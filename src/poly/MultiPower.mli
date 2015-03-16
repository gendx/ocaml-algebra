module MultiPower : sig
  type t = ((int * int) list) * int
  
  (* Args        : x *)
  (* Side-effect : print x *)
  val print: t -> unit
  (* Args        : f, n *)
  (* Side-effect : print x as a monomial with variable names given by f *)
  val print_power: (int -> string) -> t -> unit
  
  (* Args    : x *)
  (* Returns : p, the monomial where the non-null power n of variable y is given by *)
  (*               element (y, n) in the list *)
  val make: (int * int) list -> t
  
  (* Returns : x, the zero element *)
  val zero: unit -> t
  (* Args    : x *)
  (* Returns : true if x is zero *)
  (*           false otherwise *)
  val is_zero: t -> bool
  
  (* Args    : x, y *)
  (* Returns : the following result according to a compatible total order *)
  (*                 0 if x = y *)
  (*               < 0 if x < y *)
  (*               > 0 if x > y *)
  val compare: t -> t -> int
  (* Args    : x, y *)
  (* Returns : z, the element-wise addition of powers x and y *)
  val add: t -> t -> t
  (* Args    : (v, n), x *)
  (* Returns : y, the multipower x where the power of variable v has increased by n *)
  val add_var: (int * int) -> t -> t
  
  (* Args    : x, y *)
  (* Returns : g, the greatest common divisor of the monomials (x, y) *)
  val gcd: t -> t -> t
  (* Args    : x, y *)
  (* Returns : g, the greatest common divisor of the monomials (x, y) *)
  (* Args    : x, y *)
  (* Returns : (a, b) such that : *)
  (*               a = x - g *)
  (*               b = y - g *)
  (*               g is the greatest common divisor of (x, y) *)
  val gcd_reduction: t -> t -> (t * t)
end
