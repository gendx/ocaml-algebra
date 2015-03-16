open Ring

module type Field = sig
  include Ring
  
  (* Args    : x *)
  (* Returns : y, the inverse of x with respect to multiplication *)
  val inverse: t -> t
  (* Args    : x, y *)
  (* Returns : z = x.(1/y), where (1/y) is the multiplicative inverse of y *)
  val div: t -> t -> t
end
