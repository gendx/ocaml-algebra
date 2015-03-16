open EuclidianRing
open Field
open Misc

module type Quotient = sig
  type rt
  include Field with type t = rt
  
  (* Args    : x *)
  (* Returns : y, the canonical representation of x in the quotient field *)
  val make: rt -> t
end

module MakeQuotient (R : EuclidianRing) (V : Value with type t = R.t) :
  Quotient with type rt = R.t
