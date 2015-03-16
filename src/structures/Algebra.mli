open Ring
open EuclidianRing
open VectorSpace

module type Algebra = sig
  include Ring
  include VectorFunctions with type t := t
end

module type EuclidianAlgebra = sig
  include EuclidianRing
  include VectorFunctions with type t := t
end
