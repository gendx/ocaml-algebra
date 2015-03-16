module type AdditiveGroup = sig
  type t
  
  (* Args        : x *)
  (* Side-effect : print x *)
  val print: t -> unit
  
  (* Returns : x, the zero element *)
  (*               i.e. neutral element with respect to addition *)
  val zero: unit -> t
  (* Args    : x *)
  (* Returns : true if x is the zero element *)
  (*           false otherwise *)
  val is_zero: t -> bool
  
  (* Args    : x *)
  (* Returns : y, the opposite of x with respect to addition *)
  val opposite: t -> t
  (* Args    : n, x *)
  (* Returns : y, the addition of x n times *)
  val mul_int: int -> t -> t
  
  (* Args    : x, y *)
  (* Returns : z = x + y, defined by addition *)
  val add: t -> t -> t
  (* Args    : x, y *)
  (* Returns : z = x + (-y), where -y is the additive opposite of y *)
  val sub: t -> t -> t
end
