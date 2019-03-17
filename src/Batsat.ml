
(* This file is free software. See file "license" for more details. *)

type t

type 'a iter = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

(* use normal convention of positive/negative ints *)
module Lit = struct
  type t = int
  let neg n = -n
  let abs = abs
  let sign n = n > 0
  let to_int n = n
  let to_string x = (if sign x then "" else "-") ^ string_of_int (abs @@ to_int x)
  let pp out x = Format.pp_print_string out (to_string x)
  let equal : t -> t -> bool = Pervasives.(=)
  let compare : t -> t -> int = Pervasives.compare
  let hash : t -> int = Hashtbl.hash
end

type assumptions = Lit.t array

type value =
  | V_undef
  | V_true
  | V_false

let mk_val = function
  | 0 -> V_true
  | 1 -> V_false
  | 2 | 3 -> V_undef (* yep… *)
  | n -> failwith (Printf.sprintf "unknown lbool: %d" n)

let neg_value = function
  | V_true -> V_false
  | V_false -> V_true
  | V_undef -> V_undef

module TheoryArgument = struct
  type t (* opaque *)
  type lbool = int

  external value_ : t -> Lit.t -> lbool = "ml_batsat_arg_value" [@@noalloc]
  external model_size_ : t -> int = "ml_batsat_arg_model_len" [@@noalloc]
  external model_get_ : t -> int -> Lit.t = "ml_batsat_arg_model_len" [@@noalloc]
  external raise_conflict_ : t -> Lit.t array -> bool -> unit = "ml_batsat_arg_raise_conflict"

  external mk_lit : t -> Lit.t = "ml_batsat_arg_mk_lit" [@@noalloc]
  external push_lemma : t -> Lit.t array -> unit = "ml_batsat_arg_push_lemma"
  external propagate : t -> Lit.t -> unit = "ml_batsat_arg_propagate" [@@noalloc]

  let[@inline] value a lit = mk_val (value_ a lit)
  let[@inline] raise_conflict a c = raise_conflict_ a c false
  let[@inline] raise_permanent_conflict a c = raise_conflict_ a c true

  (* as a sequence *)
  let[@specialise] model_iter a yield =
    let n = model_size_ a in
    for i = 0 to n-1 do
      yield (model_get_ a i)
    done

  (* as an array *)
  let model_a a : _ array =
    Array.init (model_size_ a) (fun i -> model_get_ a i)
end

module Theory = struct
  module Arg = TheoryArgument

  (* NOTE: order and types matter a lot here! see `lib.rs/RecordField` *)
  type t = St : {
    st: 'state;
    n_levels: 'state -> int;
    push_level: 'state -> unit;
    pop_levels: 'state -> int -> unit;
    has_partial_check: bool;
    partial_check: 'state -> TheoryArgument.t -> unit;
    final_check: 'state -> TheoryArgument.t -> unit;
    explain_prop: 'state -> Lit.t -> Lit.t array;
  } -> t

  let make
      ~n_levels
      ~push_level
      ~pop_levels
      ?partial_check
      ~final_check
      ?(explain_prop=fun _ _ -> failwith "explain prop not implemented")
      st : t =
    let partial_check, has_partial_check = match partial_check with
      | None -> (fun _ _ -> ()), false
      | Some f -> f, true
    in
    St {
      st; n_levels; push_level; pop_levels; partial_check; has_partial_check;
      final_check; explain_prop; }

  (* TODO: maybe constructor with first-class module? *)
end

module Raw = struct
  type lbool = int (* 0,1,2 *)
  external create : unit -> t = "ml_batsat_new"
  external delete : t -> unit = "ml_batsat_delete"

  (* the [add_clause] functions return [false] if the clause
     immediately makes the problem unsat *)

  (* only call on a new solver, without a theory *)
  external set_th : t -> Theory.t -> unit = "ml_batsat_set_th"

  external simplify : t -> bool = "ml_batsat_simplify"

  external lit_of_int : t -> int -> Lit.t = "ml_batsat_get_lit" [@@noalloc]
  external fresh_lit : t -> Lit.t = "ml_batsat_fresh_lit" [@@noalloc]
  external add_lit : t -> Lit.t -> bool = "ml_batsat_addlit" [@@noalloc]
  external assume : t -> Lit.t -> unit = "ml_batsat_assume" [@@noalloc]
  external solve : t -> bool = "ml_batsat_solve"

  external nvars : t -> int = "ml_batsat_nvars" [@@noalloc]
  external nclauses : t -> int = "ml_batsat_nclauses" [@@noalloc]
  external nconflicts : t -> int = "ml_batsat_nconflicts" [@@noalloc]
  external ndecisions : t -> int = "ml_batsat_ndecisions" [@@noalloc]
  external nprops : t -> int = "ml_batsat_nprops" [@@noalloc]
  (*external nrestarts : t -> int "ml_batsat_nrestarts" [@@noalloc] *)

  external value : t -> Lit.t -> lbool = "ml_batsat_value" [@@noalloc]
  external check_assumption: t -> Lit.t -> bool = "ml_batsat_check_assumption" [@@noalloc]
  external unsat_core: t -> Lit.t array = "ml_batsat_unsat_core"

  external n_proved: t -> int = "ml_batsat_n_proved" [@@noalloc]
  external get_proved: t -> int -> Lit.t = "ml_batsat_get_proved" [@@noalloc]
  external value_lvl_0 : t -> Lit.t -> lbool = "ml_batsat_value_lvl_0" [@@noalloc]
end

let create () =
  let s = Raw.create() in
  Gc.finalise Raw.delete s;
  s

let delete = Raw.delete

let create_th (th:Theory.t) =
  let s = Raw.create() in
  Raw.set_th s th; (* before anything else *)
  Gc.finalise Raw.delete s;
  s

let create_th_with (f: t -> _ * Theory.t) : _ * _ =
  let s = Raw.create() in
  let data, th = f s in
  Raw.set_th s th;
  Gc.finalise Raw.delete s;
  data, s

exception Unsat

let check_ret_ b =
  if not b then raise Unsat

let add_clause_a s lits =
  Array.iter (fun x -> let r = Raw.add_lit s x in assert r) lits;
  Raw.add_lit s 0 |> check_ret_

let add_clause_l s lits =
  List.iter (fun x -> let r = Raw.add_lit s x in assert r) lits;
  Raw.add_lit s 0 |> check_ret_

let pp_clause out l =
  Format.fprintf out "[@[<hv>";
  let first = ref true in
  List.iter
    (fun x ->
       if !first then first := false else Format.fprintf out ",@ ";
       Lit.pp out x)
    l;
  Format.fprintf out "@]]"

let simplify s = Raw.simplify s |> check_ret_
let n_vars = Raw.nvars
let n_clauses = Raw.nclauses
let n_conflicts = Raw.nconflicts
let n_proved_lvl_0 = Raw.n_proved
let get_proved_lvl_0 = Raw.get_proved
(* let n_restarts = Raw.nrestarts *)
let n_props = Raw.nprops
let n_decisions = Raw.ndecisions

let lit_of_int s n =
  if n <= 0 then invalid_arg "batsat.lit_of_int";
  Raw.lit_of_int s n

let fresh_lit = Raw.fresh_lit

let proved_lvl_0 s =
  Array.init (n_proved_lvl_0 s) (get_proved_lvl_0 s)

let solve ?(assumptions=[||]) s =
  Array.iter (fun x -> Raw.assume s x) assumptions;
  Raw.solve s |> check_ret_

let is_in_unsat_core s lit = Raw.check_assumption s lit

let unsat_core = Raw.unsat_core

let string_of_value = function
  | V_undef -> "undef"
  | V_true -> "true"
  | V_false -> "false"

let pp_value out v = Format.pp_print_string out (string_of_value v)

let value s lit = mk_val @@ Raw.value s lit
let value_lvl_0 s lit = mk_val @@ Raw.value_lvl_0 s lit
