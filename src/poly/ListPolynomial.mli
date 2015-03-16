open Polynomial
open Ring
open Field

module type ListPolynomial = sig
  type ft
  include Polynomial with type ft := ft and type t = ft list
end

module type EuclidianListPolynomial = sig
  type ft
  include EuclidianPolynomial with type ft := ft and type t = ft list
end


module MakeListPolynomial (R : Ring) :
  ListPolynomial with type ft = R.t

module MakeEuclidianListPolynomial (F : Field) :
  EuclidianListPolynomial with type ft = F.t
