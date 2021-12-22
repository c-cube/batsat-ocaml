

type 'lit internal = {
  mklit: int -> 'lit;
  to_int: 'lit -> int;
  add_clause: 'lit array -> bool;
  solve: 'lit array -> bool;
}
type t = Solver : 'lit internal -> t

type builder = {
  name: string;
  make: unit -> t;
}

let mk_batsat : builder = {
  name="batsat";
  make=(fun () ->
      let module S = Batsat in
      let s = S.create() in
      let mklit i = let v = S.Lit.make (abs i) in if i>0 then v else S.Lit.neg v in
      let add_clause c = try S.add_clause_a s c; true with S.Unsat -> false in
      let solve ass = try S.solve ~assumptions:ass s; true with S.Unsat -> false in
      let to_int i = S.Lit.to_int (S.Lit.abs i) * (if S.Lit.sign i then 1 else -1) in
      Solver { add_clause; solve; mklit; to_int; }
    );
}

let name b = b.name
let all = ref [mk_batsat]

let register (b:builder) = all := b :: !all
