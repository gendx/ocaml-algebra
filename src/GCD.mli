open EuclidianRing

module GCD (R : EuclidianRing) : sig
  (* Args    : f, x, y *)
  (* Returns : z = gcd(x, y) given by Euclid's algorithm where : *)
  (*               f is a partial order on the ring *)
  val euclid: (R.t -> R.t -> bool) -> R.t -> R.t -> R.t
  (* Args    : f, x, y *)
  (* Returns : (a, b, z) where : *)
  (*               z = gcd(x, y) *)
  (*               a.x + b.y = z *)
  (*               f is a partial order on the ring *)
  val bezout: (R.t -> R.t -> bool) -> R.t -> R.t -> (R.t * R.t * R.t)
end
