
type t

type view =
  | True
  | And of t list
  | Or of t list
  | Not of t
  | Eq of Atom.t * Atom.t

val view : t -> view
val equal : t -> t -> bool
val hash : t -> int
val pp : t CCFormat.printer

val true_ : t
val false_ : t
val and_ : t list -> t
val or_ : t list -> t
val imply : t list -> t -> t
val not_ : t -> t
val eq : Atom.t -> Atom.t -> t
