type t

val of_string : string -> t

val to_string : t -> string

val of_int : int -> t

val b_and : t -> t -> t

val b_or : t -> t -> t

val lshift : t -> t -> t

val rshift : t -> t -> t

val not : t -> t
