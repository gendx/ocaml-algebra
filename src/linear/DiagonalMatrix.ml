open Matrix
open Ring
open Vector

module type DiagonalMatrix = sig
  type ft
  include SquaredMatrix with type ft := ft and type t = ft array
end


module MakeDiagonalMatrix (R : Ring) (Size : IntValue) = struct
  
  type ft = R.t
  type t = R.t array
  
  module Vector = MakeVector(R)(Size)
  
  
  let to_string (x : t) : string =
    let s = ref "" in
    for i = 0 to Size.x - 1 do
      s := !s ^
      (if i = 0 then "[" else " ");
      
      for j = 0 to Size.x - 1 do
        s := !s ^
        (if j > 0 then " " else "") ^
        (R.to_string (if i = j then (Array.get x i) else (R.zero ())))
      done;
      
      s := !s ^
      (if i = Size.x - 1 then "]" else "\n")
    done;
    !s
    
    
  let zero () : t =
    Array.make Size.x (R.zero ())
  
  let one () : t =
    Array.make Size.x (R.one ())
  
  let is_zero (x : t) : bool =
    Array.fold_left (fun result y -> result && (R.is_zero y)) true x
  
  let is_one (x : t) : bool =
    Array.fold_left (fun result y -> result && (R.is_one y)) true x
    
    
  let opposite (x : t) : t =
    Array.map R.opposite x
    
  let of_int (i : int) : t =
    Array.make Size.x (R.of_int i)
  
  let mul_int (i : int) (x : t) : t =
    Array.map (R.mul_int i) x
  
  let mul_field (lambda : R.t) (x : t) : t =
    Array.map (R.mul lambda) x
    
  
  let add (x : t) (y : t) : t =
    Array.mapi (fun i z -> R.add z (Array.get y i)) x
  
  let sub (x : t) (y : t) : t =
    Array.mapi (fun i z -> R.sub z (Array.get y i)) x
  
  let mul (x : t) (y : t) : t =
    Array.mapi (fun i z -> R.mul z (Array.get y i)) x
    
    
  let mul_vect (x : t) (v : Vector.t) : Vector.t =
    Array.mapi (fun i z -> R.mul z (Array.get v i)) x
  
end
