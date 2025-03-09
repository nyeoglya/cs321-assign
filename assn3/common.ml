module type SCALAR =
sig
  type t                               (* scalar element *)

  exception ScalarIllegal         

  val zero : t                         (* identity for ++ *)
  val one : t                          (* identity for ** *)

  val (++) : t -> t -> t               (* infix addition *)
  val ( ** ) : t -> t -> t             (* infix multiplication *)
  val (==) : t -> t -> bool            (* infix equality *)
end

module type VECTOR =
sig 
  type elem                            (* vector elements *)
  type t                               (* vector *)

  exception VectorIllegal

  val create : elem list -> t          (* creates a vector from a list *)
  val to_list : t -> elem list         (* inverse of create *)

  val dim : t -> int                   (* dimension *)
  val nth : t -> int -> elem           (* extraction *)
  val (++) : t -> t -> t               (* infix addition *)
  val (==) : t -> t -> bool            (* infix equality *)
  val innerp : t -> t -> elem          (* inner product *)
end

module type MATRIX =
sig
  type elem                            
  type t                               

  exception MatrixIllegal         

  val create : elem list list -> t     (* matrix creator *)
  val identity : int -> t              (* creates an identity matrix *)
  val dim : t -> int                   (* matrix dimension *)
  val transpose : t -> t               (* matrix transpose *)
  val to_list : t -> elem list list    (* inverse of create *)
  val get : t -> int -> int -> elem

  val (++) : t -> t -> t               (* infix addition *)
  val ( ** ) : t -> t -> t             (* infix multiplication *)
  val (==) : t -> t -> bool            (* infix equality *)
end

