
(** {1 simple sudoku solver} *)

module Fmt = CCFormat
module Vec = CCVector
module SAT = Batsat
module B_ref = Batsat_backtrackable_ref
module BLit = SAT.Lit


let debug = ref 0
let errorf msg = CCFormat.kasprintf failwith msg

module Log = struct
  let debugf lvl k : unit =
    if lvl <= !debug then (
      k (fun fmt -> Format.kfprintf
          (fun fmt -> Format.fprintf fmt "@]@.")
          Fmt.stderr fmt)
    )
end

module Cell : sig
  type t = private int
  val equal : t -> t -> bool
  val neq : t -> t -> bool
  val hash : t -> int
  val empty : t
  val is_empty : t -> bool
  val is_full : t -> bool
  val make : int -> t
  val pp : t Fmt.printer
end = struct
  type t = int
  let empty = 0
  let[@inline] make i = assert (i >= 0 && i <= 9); i
  let[@inline] is_empty x = x = 0
  let[@inline] is_full x = x > 0
  let hash = CCHash.int
  let[@inline] equal (a:t) b = a=b
  let[@inline] neq (a:t) b = a<>b
  let pp out i = if i=0 then Fmt.char out '.' else Fmt.int out i
end

module Grid : sig
  type t

  val get : t -> int -> int -> Cell.t
  val set : t -> int -> int -> Cell.t -> t

  (** A set of related cells *)
  type set = (int*int*Cell.t) Iter.t

  val rows : t -> set Iter.t
  val cols : t -> set Iter.t
  val squares : t -> set Iter.t

  val all_cells : t -> (int*int*Cell.t) Iter.t

  val parse : string -> t
  val is_full : t -> bool
  val is_valid : t -> bool
  val matches : pat:t -> t -> bool
  val pp : t Fmt.printer
end = struct
  type t = Cell.t array

  let[@inline] get (s:t) i j = s.(i*9 + j)

  let[@inline] set (s:t) i j n =
    let s' = Array.copy s in
    s'.(i*9 + j) <- n;
    s'

  (** A set of related cells *)
  type set = (int*int*Cell.t) Iter.t

  open Iter.Infix

  let all_cells (g:t) =
    0 -- 8 >>= fun i ->
    0 -- 8 >|= fun j -> (i,j,get g i j)

  let rows (g:t) =
    0 -- 8 >|= fun i ->
    ( 0 -- 8 >|= fun j -> (i,j,get g i j))

  let cols g =
    0 -- 8 >|= fun j ->
    ( 0 -- 8 >|= fun i -> (i,j,get g i j))

  let squares g =
    0 -- 2 >>= fun sq_i ->
    0 -- 2 >|= fun sq_j ->
    ( 0 -- 2 >>= fun off_i ->
      0 -- 2 >|= fun off_j ->
      let i = 3*sq_i + off_i in
      let j = 3*sq_j + off_j in
      (i,j,get g i j))

  let is_full g = Array.for_all Cell.is_full g

  let is_valid g =
    let all_distinct (s:set) =
      (s >|= fun (_,_,c) -> c)
      |> Iter.diagonal
      |> Iter.for_all (fun (c1,c2) -> Cell.neq c1 c2)
    in
    Iter.for_all all_distinct @@ rows g &&
    Iter.for_all all_distinct @@ cols g &&
    Iter.for_all all_distinct @@ squares g

  let matches ~pat:g1 g2 : bool =
    all_cells g1
    |> Iter.filter (fun (_,_,c) -> Cell.is_full c)
    |> Iter.for_all (fun (x,y,c) -> Cell.equal c @@ get g2 x y)

  let pp out g =
    Fmt.fprintf out "@[<v>";
    Array.iteri
      (fun i n ->
         Cell.pp out n;
         if i mod 9 = 8 then Fmt.fprintf out "@,")
      g;
    Fmt.fprintf out "@]"

  let parse (s:string) : t =
    if String.length s < 81 then (
      errorf "line is too short, expected 81 chars, not %d" (String.length s);
    );
    let a = Array.make 81 Cell.empty in
    for i = 0 to 80 do
      let c = String.get s i in
      let n = if c = '.' then 0 else Char.code c - Char.code '0' in
      if n < 0 || n > 9 then errorf "invalid char %c" c;
      a.(i) <- Cell.make n
    done;
    a
end

module Solver
(*
  : sig
  type t
  val create : Grid.t -> t
  val solve : t -> Grid.t option
end 
*)
    (* FIXME *)
= struct
  (* formulas *)
  module F = struct
    type t = bool*int*int*Cell.t
    let equal (sign1,x1,y1,c1)(sign2,x2,y2,c2) =
      sign1=sign2 && x1=x2 && y1=y2 && Cell.equal c1 c2
    let hash (sign,x,y,c) = CCHash.(combine4 (bool sign)(int x)(int y)(Cell.hash c))
    let pp out (sign,x,y,c) =
      Fmt.fprintf out "[@[(%d,%d) %s %a@]]" x y (if sign then "=" else "!=") Cell.pp c
    let neg (sign,x,y,c) = (not sign,x,y,c)
    let make sign x y (c:Cell.t) : t = (sign,x,y,c)
    let abs (_,x,y,c) = make true x y c
    let sign (s,_,_,_) = s
    let apply_sign s f = if s then f else neg f
  end

  module F_tbl = CCHashtbl.Make(F)
  module BLit_tbl = CCHashtbl.Make(BLit)

  module Lit_map = struct
    type t = {
      to_lit: BLit.t F_tbl.t;
      of_lit: F.t BLit_tbl.t;
      sat: SAT.t;
    }

    let create (sat:SAT.t) : t =
      {sat; to_lit=F_tbl.create 32; of_lit =BLit_tbl.create 32}

    let lit_of_f (self:t) (f:F.t) : BLit.t =
      (*Log.debugf 15 (fun k->k "(@[lit-of-form@ %a@])" F.pp f); *)
      let sign = F.sign f in
      let lit =
        let f_abs = F.abs f in
        try F_tbl.find self.to_lit f_abs
        with Not_found ->
          let lit = SAT.fresh_lit self.sat in
          F_tbl.add self.to_lit f_abs lit;
          BLit_tbl.add self.of_lit lit f_abs;
          lit
      in
      if sign then lit else BLit.neg lit

    let f_of_lit (self:t) (lit:BLit.t) : F.t =
      (*Log.debugf 15 (fun k->k "(@[form-of-lit@ %a@])" BLit.pp lit);*)
      let sign = BLit.sign lit in
      try F.apply_sign sign @@ BLit_tbl.find self.of_lit (BLit.abs lit)
      with Not_found -> errorf "no formula for literal %a" BLit.pp lit
  end

  module Theory = struct
    module Formula = F
    type t = {
      grid: Grid.t B_ref.t;
      lm: Lit_map.t;
    }

    let create lm g : t = {grid=B_ref.create g; lm}
    let[@inline] grid self : Grid.t = B_ref.get self.grid
    let[@inline] set_grid self g : unit = B_ref.set self.grid g

    let push_level self = B_ref.push_level self.grid
    let pop_levels self n = B_ref.pop_levels self.grid n
    let n_levels self = B_ref.n_levels self.grid

    let pp_c_ = Fmt.(array ~sep:(return "@ ∨ ")) F.pp
    let[@inline] logs_conflict kind c : unit =
      Log.debugf 4 (fun k->k "(@[conflict.%s@ %a@])" kind pp_c_ c)

    (* check that all cells are full *)
    let check_full_ (self:t) (acts:SAT.TheoryArgument.t) : unit =
      Grid.all_cells (grid self)
        (fun (x,y,c) ->
           if Cell.is_empty c then (
             let c =
               Array.init 9
                 (fun c -> F.make true x y (Cell.make (c+1)))
             in
             Log.debugf 4 (fun k->k "(@[add-clause@ %a@])" pp_c_ c);
             let c = Array.map (Lit_map.lit_of_f self.lm) c in
             SAT.TheoryArgument.push_lemma acts c;
           ))

    (* check constraints *)
    let check_ (self:t) acts : unit =
      Log.debugf 4 (fun k->k "(@[sudoku.check@ @[:g %a@]@])" Grid.pp (B_ref.get self.grid));
      let[@inline] all_diff kind f =
        let pairs =
          f (grid self)
          |> Iter.flat_map
            (fun set ->
               set
               |> Iter.filter (fun (_,_,c) -> Cell.is_full c)
               |> Iter.diagonal)
        in
        pairs
          (fun ((x1,y1,c1),(x2,y2,c2)) ->
             if Cell.equal c1 c2 then (
               assert (x1<>x2 || y1<>y2);
               let c = [|F.make false x1 y1 c1; F.make false x2 y2 c2|] in
               logs_conflict ("all-diff." ^ kind) c;
               let c = Array.map (Lit_map.lit_of_f self.lm) c in
               SAT.TheoryArgument.raise_conflict acts c;
             ))
      in
      all_diff "rows" Grid.rows;
      all_diff "cols" Grid.cols;
      all_diff "squares" Grid.squares;
      Log.debugf 4 (fun k->k "(sudoku.check.done)");
      ()

    let trail_ (self:t) (acts:SAT.TheoryArgument.t) = 
      SAT.TheoryArgument.model_iter acts
      |> Iter.map (Lit_map.f_of_lit self.lm)

    (* update current grid with the given slice *)
    let add_slice (self:t) acts : unit =
      trail_ self acts
        (function
          | false,_,_,_ -> ()
          | true,x,y,c ->
            assert (Cell.is_full c);
            let grid = grid self in
            let c' = Grid.get grid x y in
            if Cell.is_empty c' then (
              set_grid self (Grid.set grid x y c);
            ) else if Cell.neq c c' then (
              (* conflict: at most one value *)
              let c = [|F.make false x y c; F.make false x y c'|] in
              logs_conflict "at-most-one" c;
              let c = Array.map (Lit_map.lit_of_f self.lm) c in
              SAT.TheoryArgument.raise_conflict acts c;
            )
        )

    let partial_check (self:t) acts : unit =
      Log.debugf 4 (fun k->k "(sudoku.partial-check)");
      Log.debugf 15
        (fun k->k "(@[cur-trail [@[%a@]]@])" (Fmt.seq F.pp) (trail_ self acts));
      add_slice self acts;
      check_ self acts;
      ()

    let final_check (self:t) acts : unit =
      Log.debugf 4 (fun k->k "(@[sudoku.final-check@])");
      check_full_ self acts;
      check_ self acts;
      ()
  end

  let mk_theory g sat : Theory.t * SAT.Theory.t =
    let th = Theory.create sat g in
    th, SAT.Theory.make
      ~pop_levels:Theory.pop_levels
      ~push_level:Theory.push_level
      ~n_levels:Theory.n_levels
      ~partial_check:Theory.partial_check
      ~final_check:Theory.final_check
      th

  type t = {
    grid0: Grid.t;
    solver: SAT.t;
    lm: Lit_map.t;
    th: Theory.t;
  }

  let solve (self:t) : _ option =
    let assumptions =
      Grid.all_cells self.grid0
      |> Iter.filter (fun (_,_,c) -> Cell.is_full c)
      |> Iter.map (fun (x,y,c) -> F.make true x y c)
      |> Iter.map (Lit_map.lit_of_f self.lm)
      |> Iter.to_array
    in
    Log.debugf 2
      (fun k->k "(@[sudoku.solve@ :assumptions %a@])" (Fmt.Dump.array BLit.pp) assumptions);
    let r =
      match SAT.solve self.solver ~assumptions with
      | () -> Some (Theory.grid self.th)
      | exception SAT.Unsat -> None
    in
    (* TODO: print some stats *)
    r

  let create (g:Grid.t) : t =
    let (lm,th), solver =
      SAT.create_th_with
        (fun sat ->
             let lm = Lit_map.create sat in
             let th, sat_th = mk_theory g lm in
             (lm,th), sat_th)
    in
    { solver; th; lm; grid0=g }
end

let solve_grid (g:Grid.t) : Grid.t option =
  let s = Solver.create g in
  Solver.solve s

let solve_file file =
  Format.printf "solve grids in file %S@." file;
  let start = Sys.time() in
  let grids =
    CCIO.with_in file CCIO.read_lines_l
    |> CCList.filter_map
      (fun s ->
         let s = String.trim s in
         if s="" then None
         else match Grid.parse s with
           | g -> Some g
           | exception e ->
             errorf "cannot parse sudoku %S: %s@." s (Printexc.to_string e))
  in
  Format.printf "parsed %d grids (in %.3fs)@." (List.length grids) (Sys.time()-.start);
  List.iter
    (fun g ->
       Format.printf "@[<v>@,#########################@,@[<2>solve grid:@ %a@]@]@." Grid.pp g;
       let start=Sys.time() in
       match solve_grid g with
       | None -> Format.printf "no solution (in %.3fs)@." (Sys.time()-.start)
       | Some g' when not @@ Grid.is_full g' ->
         errorf "grid %a@ is not full" Grid.pp g'
       | Some g' when not @@ Grid.is_valid g' ->
         errorf "grid %a@ is not valid" Grid.pp g'
       | Some g' when not @@ Grid.matches ~pat:g g' ->
         errorf "grid %a@ @[<2>does not match original@ %a@]" Grid.pp g' Grid.pp g
       | Some g' ->
         Format.printf "@[<v>@[<2>solution (in %.3fs):@ %a@]@,###################@]@."
           (Sys.time()-.start) Grid.pp g')
    grids;
  Format.printf "@.solved %d grids (in %.3fs)@." (List.length grids) (Sys.time()-.start);
  ()

let () =
  Fmt.set_color_default true;
  let files = ref [] in
  let opts = [
    "--debug", Arg.Set_int debug, " debug";
    "-d", Arg.Set_int debug, " debug";
  ] |> Arg.align in
  Arg.parse opts (fun f -> files := f :: !files) "sudoku_solve [options] <file>";
  try
    List.iter (fun f -> solve_file f) !files;
  with
  | Failure msg | Invalid_argument msg ->
    Format.printf "@{<Red>Error@}:@.%s@." msg;
    exit 1
