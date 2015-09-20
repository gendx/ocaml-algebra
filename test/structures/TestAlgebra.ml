(*
    OCaml-algebra - an algebra system written in OCaml.
    Copyright (C) 2015  G. Endignoux

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see http://www.gnu.org/licenses/gpl-3.0.txt
*)

open OUnit
open Ring
open Algebra
open TestRing
open TestEuclidianRing
open TestVectorSpace

module TestAlgebra (R : Ring) (A : Algebra with type ft = R.t) = struct

  module TestRingImpl = TestRing(A)
  module TestVectorSpaceImpl = TestVectorSpace(R)(A)

  let test (x : A.t) (y : A.t) (z : A.t) (f : R.t) (g : R.t) : OUnit.test list =
    (TestVectorSpaceImpl.test_me x y z f g)::(TestRingImpl.test x y z)

end

module TestCommutativeAlgebra (R : Ring) (A : Algebra with type ft = R.t) = struct

  module TestCommutativeRingImpl = TestCommutativeRing(A)
  module TestVectorSpaceImpl = TestVectorSpace(R)(A)

  let test (x : A.t) (y : A.t) (z : A.t) (f : R.t) (g : R.t) : OUnit.test list =
    (TestVectorSpaceImpl.test_me x y z f g)::(TestCommutativeRingImpl.test x y z)

end

module TestEuclidianAlgebra (R : Ring) (A : EuclidianAlgebra with type ft = R.t) = struct

  module TestEuclidianRingImpl = TestEuclidianRing(A)
  module TestVectorSpaceImpl = TestVectorSpace(R)(A)

  let test (x : A.t) (y : A.t) (z : A.t) (f : R.t) (g : R.t) : OUnit.test list =
    (TestVectorSpaceImpl.test_me x y z f g)::(TestEuclidianRingImpl.test x y z)

end

