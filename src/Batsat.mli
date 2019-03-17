
(* This file is free software. See file "license" for more details. *)

(** {1 Bindings to Batsat} *)

type t
(** An instance of batsat (stateful) *)

type 'a iter = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

module Backtrackable_ref = Batsat_backtrackable_ref

module Lit : sig
  type t = private int
  (** Some representation of literals that will be accepted by the SAT solver. *)

  val equal : t -> t -> bool
  val hash : t -> int
  val compare : t -> t -> int

  val neg : t -> t
  (** Negation of a literal.
      Invariant: [neg (neg x) = x] *)

  val abs : t -> t
  (** Absolute value (removes negation if any). *)

  val sign : t -> bool
  (** Sign: [true] if the literal is positive, [false] for a negated literal.
      Invariants:
      [sign (abs x) = true]
      [sign (neg x) = not (sign x)]
  *)

  val to_int : t -> int
  val to_string : t -> string
  val pp : t printer
end

type value =
  | V_undef
  | V_true
  | V_false

val pp_value : value printer
val string_of_value : value -> string
val neg_value : value -> value

(** Passed to the Theory *)
module TheoryArgument : sig
  type t

  val value : t -> Lit.t -> value
  val model_a : t -> Lit.t array
  val model_iter : t -> Lit.t iter
  val push_lemma : t -> Lit.t array -> unit
  val raise_permanent_conflict : t -> Lit.t array -> 'a
  val raise_conflict : t -> Lit.t array -> 'a
  val mk_lit : t -> Lit.t
  val propagate : t -> Lit.t -> unit
end

module Theory : sig
  module Arg = TheoryArgument

  type t

  val make:
    n_levels:('state -> int) ->
    push_level:('state -> unit) ->
    pop_levels:('state -> int -> unit) ->
    ?partial_check:('state -> TheoryArgument.t -> unit) ->
    final_check:('state -> TheoryArgument.t -> unit) ->
    ?explain_prop:('state -> int -> int array) ->
    'state -> t
end

type assumptions = Lit.t array

val create : unit -> t

val create_th : Theory.t -> t

val create_th_with : (t -> 'a * Theory.t) -> 'a * t
(** More general builder for recursive theory and solver *)

val delete : t -> unit
(** Release resources *)

exception Unsat

val add_clause_l : t -> Lit.t list -> unit
(** @raise Unsat if the problem is unsat *)

val add_clause_a : t -> Lit.t array -> unit
(** @raise Unsat if the problem is unsat *)

val pp_clause : Lit.t list printer

val fresh_lit : t -> Lit.t
(** Allocate a fresh literal *)

val lit_of_int : t -> int -> Lit.t
(** [lit_of_int n] allocates literals until one whose index is [n] is reached.
    Use {!Lit.neg} to obtain the negation of a literal.
    @raise Invalid_argument if the integer is not positive.
*)

val simplify : t -> unit
(** @raise Unsat if the problem is unsat *)

val solve : ?assumptions:assumptions -> t -> unit
(** @raise Unsat if the problem is unsat *)

val n_vars : t -> int
val n_clauses : t -> int
val n_conflicts : t -> int

val n_props : t -> int
(** Number of SAT propagations
    @since NEXT_RELEASE *)

val n_decisions : t -> int
(** Number of SAT decisions
    @since NEXT_RELEASE *)

val is_in_unsat_core : t -> Lit.t -> bool
(** [is_in_unsat_core s lit] checks whether [abs(lit)] is part of the
    unsat core (if it was an assumption)
    precondition: last call to {!solve} raised {!Unsat} *)

val unsat_core : t -> Lit.t array
(** Access the whole unsat core
    precondition: last call to {!solve} raised {!Unsat} *)

val n_proved_lvl_0 : t -> int
(** Number of literals true at level0 (ie proved unconditionally).
    Can only grow. *)

val get_proved_lvl_0 : t -> int -> Lit.t
(** Get the n-th proved literal *)

val proved_lvl_0 : t -> Lit.t array
(** All literals currently proved at level 0 *)

val value : t -> Lit.t -> value

val value_lvl_0 : t -> Lit.t -> value
(** [value_lvl_0 solver lit] returns the value of [lit] if it has this
    value at level 0 (proved), or [V_undef] otherwise *)
