open EuclidianRing
open Field

module type Fractional = sig
  type rt
  include Field with type t = rt * rt
  
  (* Args    : x, y *)
  (* Returns : z = xx/yy, such that : *)
  (*               xx is the canonical representation of x in the fractional field *)
  (*               yy is the canonical representation of x in the fractional field *)
  val make: rt -> rt -> t
  (* Args    : x, y *)
  (* Returns : z = xx/yy, such that : *)
  (*               xx is the canonical representation of x in the fractional field *)
  (*               yy is the canonical representation of x in the fractional field *)
  val make_of_int: int -> int -> t
end

module MakeFractional (R : EuclidianRing) :
  Fractional with type rt = R.t
