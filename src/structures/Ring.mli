open Group

module type Ring = sig
  include AdditiveGroup
  
  (* Returns : x, the one element *)
  (*               i.e. neutral element with respect to multiplication *)
  val one: unit -> t
  (* Args    : x *)
  (* Returns : true if x is the one element *)
  (*           false otherwise *)
  val is_one: t -> bool
  
  (* Args    : n *)
  (* Returns : x, the addition of the one element n times *)
  val of_int: int -> t
  
  (* Args    : x, y *)
  (* Returns : z = x.y, defined by multiplication *)
  val mul: t -> t -> t
end
