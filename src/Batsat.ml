
(* This file is free software. See file "license" for more details. *)

type 'a iter = ('a -> unit) -> unit
type 'a printer = Format.formatter -> 'a -> unit

(** A backtrackable reference *)
module Backtrackable_ref = Batsat_backtrackable_ref

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

exception Exit_for_conflict

module TheoryArgument = struct
  type t (* opaque *)
  type lbool = int

  external value_ : t -> Lit.t -> lbool = "ml_batsat_arg_value" [@@noalloc]
  external model_size_ : t -> int = "ml_batsat_arg_model_len" [@@noalloc]
  external model_get_ : t -> int -> Lit.t = "ml_batsat_arg_model_get" [@@noalloc]
  external raise_conflict_ : t -> Lit.t array -> bool -> unit = "ml_batsat_arg_raise_conflict"

  external mk_lit : t -> Lit.t = "ml_batsat_arg_mk_lit" [@@noalloc]
  external push_lemma : t -> Lit.t array -> unit = "ml_batsat_arg_push_lemma"
  external propagate : t -> Lit.t -> unit = "ml_batsat_arg_propagate" [@@noalloc]

  let[@inline] value a lit = mk_val (value_ a lit)
  let[@inline] raise_conflict a c = raise_conflict_ a c false; raise_notrace Exit_for_conflict
  let[@inline] raise_permanent_conflict a c = raise_conflict_ a c true; raise_notrace Exit_for_conflict

  (* TODO: use a backtrackable ref to only provide new literals in partial_check *)

  (* as a sequence *)
  let[@specialise] model_iter a yield =
    let n = model_size_ a in
    for i = 0 to n-1 do
      let lit = model_get_ a i in
      yield lit
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
      | Some f ->
        let f' st a = try f st a with Exit_for_conflict -> () in
        f', true
    in
    let final_check st a = try final_check st a with Exit_for_conflict -> () in
    St {
      st; n_levels; push_level; pop_levels; partial_check; has_partial_check;
      final_check; explain_prop; }

  (* TODO: maybe constructor with first-class module? *)
end

module Raw = struct
  type t
  type lbool = int (* 0,1,2 *)
  external create : unit -> t = "ml_batsat_new"
  external delete : t -> unit = "ml_batsat_delete"

  (* the [add_clause] functions return [false] if the clause
     immediately makes the problem unsat *)

  external simplify : t -> bool = "ml_batsat_simplify"

  external lit_of_int : t -> int -> Lit.t = "ml_batsat_get_lit" [@@noalloc]
  external fresh_lit : t -> Lit.t = "ml_batsat_fresh_lit" [@@noalloc]
  external add_lit : t -> Lit.t -> bool = "ml_batsat_addlit" [@@noalloc]
  external assume : t -> Lit.t -> unit = "ml_batsat_assume" [@@noalloc]
  external solve : t -> bool = "ml_batsat_solve"
  external solve_th : t -> Theory.t -> bool = "ml_batsat_solve_th"

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

type t = {
  solver: Raw.t;
  mutable th: Theory.t option;
}

let create () : t =
  let solver = Raw.create() in
  Gc.finalise Raw.delete solver;
  {solver; th=None; }

let delete s = Raw.delete s.solver

let create_th (th:Theory.t) : t =
  let solver = Raw.create() in
  Gc.finalise Raw.delete solver;
  {solver; th=Some th}

let create_th_with (f: t -> _ * Theory.t) : _ * _ =
  let s = create() in
  let data, th = f s in
  s.th <- Some th;
  data, s

exception Unsat

let check_ret_ b =
  if not b then raise Unsat

let add_clause_a (s:t) lits =
  Array.iter (fun x -> let r = Raw.add_lit s.solver x in assert r) lits;
  Raw.add_lit s.solver 0 |> check_ret_

let add_clause_l (s:t) lits =
  List.iter (fun x -> let r = Raw.add_lit s.solver x in assert r) lits;
  Raw.add_lit s.solver 0 |> check_ret_

let pp_clause out l =
  Format.fprintf out "[@[<hv>";
  let first = ref true in
  List.iter
    (fun x ->
       if !first then first := false else Format.fprintf out ",@ ";
       Lit.pp out x)
    l;
  Format.fprintf out "@]]"

let[@inline] simplify (s:t) = Raw.simplify s.solver |> check_ret_
let[@inline] n_vars (s:t) = Raw.nvars s.solver
let[@inline] n_clauses (s:t) = Raw.nclauses s.solver
let[@inline] n_conflicts (s:t) = Raw.nconflicts s.solver
let[@inline] n_proved_lvl_0 (s:t) = Raw.n_proved s.solver
let[@inline] get_proved_lvl_0 (s:t) = Raw.get_proved s.solver
(* let n_restarts = Raw.nrestarts *)
let[@inline] n_props (s:t) = Raw.nprops s.solver
let[@inline] n_decisions (s:t) = Raw.ndecisions s.solver

let lit_of_int (s:t)  n =
  if n <= 0 then invalid_arg "batsat.lit_of_int";
  Raw.lit_of_int s.solver n

let fresh_lit (s:t) = Raw.fresh_lit s.solver

let proved_lvl_0 (s:t)  =
  Array.init (n_proved_lvl_0 s) (get_proved_lvl_0 s)

let solve ?(assumptions=[||]) (s:t)  =
  Array.iter (fun x -> Raw.assume s.solver x) assumptions;
  let res = match s.th with
    | None -> Raw.solve s.solver
    | Some th -> Raw.solve_th s.solver th
  in
  check_ret_ res

let[@inline] is_in_unsat_core (s:t)  lit = Raw.check_assumption s.solver lit
let[@inline] unsat_core (s:t) = Raw.unsat_core s.solver

let string_of_value = function
  | V_undef -> "undef"
  | V_true -> "true"
  | V_false -> "false"

let pp_value out v = Format.pp_print_string out (string_of_value v)

let value (s:t)  lit = mk_val @@ Raw.value s.solver lit
let value_lvl_0 (s:t)  lit = mk_val @@ Raw.value_lvl_0 s.solver lit
