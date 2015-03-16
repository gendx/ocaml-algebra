open Algebra
open Vector

module type SquaredMatrix = sig
  type ft
  type t
  
  include Algebra with type ft := ft and type t := t
  
  module Vector : Vector with type ft := ft
  (* Args    : x, y *)
  (* Returns : z, the product of x and y *)
  val mul_vect: t -> Vector.t -> Vector.t
end
