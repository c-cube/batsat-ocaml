
type t

val create : unit -> t

val process_stmt : t -> Pb.stmt -> unit
