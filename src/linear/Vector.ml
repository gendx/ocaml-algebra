open VectorSpace
open Ring

module type IntValue = sig val x: int end

module type Vector = sig
  type ft
  type t = ft array
  
  include VectorSpace with type ft := ft and type t := t
  
  (* Args    : x, y *)
  (* Returns : z, the dot product of x and y *)
  val dot: t -> t -> ft
end


module MakeVector (R : Ring) (Size : IntValue) = struct
  
  type ft = R.t
  type t = R.t array
  
  
  let print (x : t) : unit =
    print_string "[";
    for i = 0 to Size.x - 1 do
      if i > 0 then
        print_string " ";
      
      R.print (Array.get x i)
    done;
    print_string "]"
    
    
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
    
    
  let dot (x : t) (y : t) : ft =
    let result, _ = Array.fold_left
      (fun (result, i) z ->
          (R.add result (R.mul z (Array.get y i)), i + 1)
      )
    (R.zero (), 0) x
    in result
  
end
