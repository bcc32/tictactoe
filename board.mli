open Core.Std

module Mark : sig
  type t =
    | X
    | O
  [@@deriving bin_io, sexp]

  val to_string : t -> string
  val next      : t -> t
end

type t [@@deriving bin_io, sexp]

val create : int -> t
val get  : t -> x:int -> y:int -> Mark.t option
val set  : t -> x:int -> y:int -> Mark.t -> unit Or_error.t
val fset : t -> x:int -> y:int -> Mark.t -> t    Or_error.t

val eval : t -> player:Mark.t -> int
val is_end : t -> bool
val winner : t -> Mark.t option

val to_string : t -> string
