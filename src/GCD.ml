open EuclidianRing

module GCD (R : EuclidianRing) = struct
  
  let euclid (lt : R.t -> R.t -> bool) (x : R.t) (y : R.t) : R.t =
    let rec euclid_impl_core x y =
      if R.is_zero x then
        y
      else
        euclid_impl_core (R.rem y x) x
    in
    
    let euclid_impl x y =
      (* x = y = 0 *)
      if R.is_zero y then
        R.one ()
      else
        euclid_impl_core x y
    in
    
    (* assert x < y *)
    if lt y x then
      euclid_impl y x
    else
      euclid_impl x y
  
  let bezout (lt : R.t -> R.t -> bool) (x : R.t) (y : R.t) : (R.t * R.t * R.t) =
    let rec bezout_impl_core x y a0 b0 a1 b1 =
      if R.is_zero x then
        (a0, b0, y)
      else (
        let q, r = R.quorem y x in
        bezout_impl_core r x a1 b1 (R.sub a0 (R.mul q a1)) (R.sub b0 (R.mul q b1))
      )
    in
    
    let bezout_impl x y a0 b0 a1 b1 =
      (* x = y = 0 *)
      if R.is_zero y then
        (R.one (), R.zero (), R.one ())
      else
        bezout_impl_core x y a0 b0 a1 b1
    in
    
    if lt y x then (
      let b, a, gcd = bezout_impl y x (R.zero ()) (R.one ()) (R.one ()) (R.zero ()) in
      (a, b, gcd)
    )	else
      bezout_impl x y (R.zero ()) (R.one ()) (R.one ()) (R.zero ())
  
end
