open Common

exception NotImplemented

exception IllegalFormat 

module Integer : SCALAR

module Boolean : SCALAR

module VectorFn (Scal : SCALAR) : VECTOR with type elem = Scal.t

module MatrixFn (Scal : SCALAR) : MATRIX with type elem = Scal.t

module ClosureFn (Mat : MATRIX) : 
  sig
    val closure : Mat.t -> Mat.t
  end

module BoolMat : MATRIX with type elem = bool

module BoolMatClosure : 
  sig
    val closure : BoolMat.t -> BoolMat.t
  end

val reach : bool list list -> bool list list
val al : bool list list
val solution_al' : bool list list

val distance : int list list -> int list list
val dl : int list list
val solution_dl' : int list list

val weight : int list list -> int list list
val ml : int list list
val solution_ml' : int list list

