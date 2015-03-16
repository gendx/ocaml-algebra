open Polynomial
open Ring
open Field

module type SparsePolynomial = sig
  type ft
  include Polynomial with type ft := ft and type t = (ft * int) list
end

module type EuclidianSparsePolynomial = sig
  type ft
  include EuclidianPolynomial with type ft := ft and type t = (ft * int) list
end


module MakeSparsePolynomial (R : Ring) :
  SparsePolynomial with type ft = R.t

module MakeEuclidianSparsePolynomial (F : Field) :
  EuclidianSparsePolynomial with type ft = F.t
