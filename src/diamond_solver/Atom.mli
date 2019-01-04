
type t
val make : string -> t
val equal : t -> t -> bool
val compare : t -> t -> int
val hash : t -> int

val pp : t CCFormat.printer
