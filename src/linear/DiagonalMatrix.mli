open Matrix
open Ring
open Vector

module type DiagonalMatrix = sig
  type ft
  include SquaredMatrix with type ft := ft and type t = ft array
end


module MakeDiagonalMatrix (R : Ring) (Size : IntValue) :
  DiagonalMatrix with type ft = R.t
