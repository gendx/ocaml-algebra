open Ring

module type EuclidianRing = sig
  include Ring
  
  (* Args    : x *)
  (* Returns : u, a unit element such that : *)
  (*               x.u is normalized *)
  val unit_part: t -> t
  (* Args    : x *)
  (* Returns : (y, u) such that : *)
  (*               y = normalized(x) *)
  (*               y = x.u *)
  val normalize: t -> (t * t)
  
  (* Args    : x, y *)
  (* Returns : (q, r) such that : *)
  (*               x = q.y + r is the euclidian division of x by y *)
  val quorem: t -> t -> (t * t)
  (* Args    : x, y *)
  (* Returns : q, the quotient of the euclidian division of x by y *)
  val quo: t -> t -> t
  (* Args    : x, y *)
  (* Returns : r, the remainder of the euclidian division of x by y *)
  val rem: t -> t -> t
  (* Args    : x, y *)
  (* Returns : g, a greatest common divisor of (x, y) *)
  val gcd: t -> t -> t
  (* Args    : x, y *)
  (* Returns : (qx, qy) such that : *)
  (*               g is the normalized greatest common divisor of (x, y) *)
  (*               x = g.qx *)
  (*               y = g.qy *)
  val gcd_reduction: t -> t -> (t * t)
  (* Args    : x, y *)
  (* Returns : (a, b, g) such that : *)
  (*               g is a greatest common divisor of (x, y) *)
  (*               a.x + b.y = g *)
  val bezout: t -> t -> (t * t * t)
end
