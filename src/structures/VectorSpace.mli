open Group

module type VectorFunctions = sig
  type ft
  type t
  
  (* Args    : l, x *)
  (* Returns : z = l.x, defined by external product *)
  val mul_field: ft -> t -> t
end

module type VectorSpace = sig
  include AdditiveGroup
  include VectorFunctions with type t := t
end
