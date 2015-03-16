open VectorSpace
open Ring

module type IntValue = sig val x: int end

module type Vector = sig
  type ft
  type t = ft array
  
  include VectorSpace with type ft := ft and type t := t
  
  (* Args    : x, y *)
  (* Returns : z, the dot product of x and y *)
  val dot: t -> t -> ft
end


module MakeVector (R : Ring) (Size : IntValue) :
  Vector with type ft = R.t
