open Matrix
open Ring
open Vector

module type ArrayMatrix = sig
  type ft
  include SquaredMatrix with type ft := ft and type t = ft array array
  
  (* Args    : x, y *)
  (* Returns : m, the matrix given by the tensorial product of x and y *)
  (*               i.e. m = x.y^T *)
  val of_vects: Vector.t -> Vector.t -> t
end


module MakeArrayMatrix (R : Ring) (Size : IntValue) :
  ArrayMatrix with type ft = R.t
