
module B = Batsat

module Parse : sig
  type 'a event =
    | Add_clause of 'a array

  type 'a t

  val make : file:string -> (int -> 'a) -> 'a t

  val iter : 'a t -> ('a event -> unit) -> unit
end = struct
  module Vec = CCVector
  module L = Lexer

  type 'a event =
    | Add_clause of 'a array

  type 'a t = {
    mk: int -> 'a;
    vec: 'a Vec.vector;
    lex: Lexing.lexbuf;
  }

  let make ~file mk : _ t =
    let ic = open_in file in
    let lex = Lexing.from_channel ic in
    at_exit (fun () -> close_in_noerr ic);
    {lex; vec=Vec.create(); mk; }

  let read_ints ?first self : _ array =
    Vec.clear self.vec; (* reuse local vec *)
    CCOpt.iter (Vec.push self.vec) first;
    let rec aux() =
      match L.token self.lex with
      | L.I 0 -> Vec.to_array self.vec (* done *)
      | L.I n ->
        let x = self.mk n in
        Vec.push self.vec x;
        aux()
      | L.EOF -> failwith "unexpected end of file"
    in
    aux()

  let rec iter (self:_ t) f : unit =
    match L.token self.lex with
    | L.EOF -> ()
    | L.I 0 ->
      f (Add_clause [| |]); iter self f
    | L.I x ->
      let c = read_ints ~first:(self.mk x) self in
      f (Add_clause c); iter self f
end

let solve_file ~debug file : unit =
  let solver = B.create() in
  let parse =
    Parse.make ~file (fun i ->
        let a = abs i in
        B.Lit.make_with_sign (i>0) a
      ) in
  try
    Parse.iter parse
      (function
        | Parse.Add_clause c ->
          if debug then Format.eprintf "add clause %a@." B.pp_clause (Array.to_list c);
          B.add_clause_a solver c);

    B.solve solver;
    Format.printf "sat@."
  with B.Unsat ->
    Format.printf "unsat@."

let () =
  let files = ref [] in
  let debug = ref false in
  let opts = [
    "-d", Arg.Set debug, " debug";
  ] |> Arg.align in
  Arg.parse opts (fun f -> files := f :: !files) "solver [options] <file>";
  List.iter (fun f -> solve_file ~debug:!debug f) !files;
  ()
