
(** Basic Diamond problem solver *)

type sexp = Sexp.t

module Parse = struct
  let atom s : Atom.t = match s with
    | `Atom s -> Atom.make s
    | _ -> failwith @@ "Expected atom, got " ^ Sexp.to_string s

  let rec term (s:sexp) : Term.t =
    match s with
    | `Atom "true" -> Term.true_
    | `Atom "false" -> Term.false_
    | `List [`Atom "="; a; b] -> Term.eq (atom a) (atom b)
    | `List (`Atom "and" :: l) -> Term.and_ @@ List.map term l
    | `List (`Atom "or" :: l) -> Term.and_ @@ List.map term l
    | `List [`Atom "=>"; a; b] -> Term.imply [term a] (term b)
    | `List [`Atom "not"; a] -> Term.not_ (term a)
    | _ ->
      failwith @@ "Expected term, got " ^ Sexp.to_string s

  let stmt (s:sexp) : Pb.stmt =
    match s with
    | `List [`Atom "exit"] -> Pb.Exit
    | `List [`Atom "check-sat"] -> Pb.Check_sat
    | `List [`Atom "assert"; t] -> Pb.Assert (term t)
    | `List [`Atom "declare-sort"; f; `Atom n] ->
      let n = int_of_string n in
      Pb.Declare_sort (atom f, n)
    | `List [`Atom "declare-fun"; f; `List args; res] ->
      Pb.Declare_fun (atom f, List.map atom args, atom res)
    | `List [`Atom "set-logic"; `Atom s] -> Pb.SetLogic s
    | `List [`Atom "set-info"; `Atom a; `Atom b] -> Pb.SetInfo (a,b)
    | _ ->
      failwith @@ "Expected statement, got " ^ Sexp.to_string s

  let pb (l: sexp list) : Pb.t = List.map stmt l
end

let process_file f =
  Format.printf "## process file %S@." f;
  let sexp_l =
    CCIO.with_in f (fun ic ->
        match Sexp.parse_chan_list ic with
        | Error msg -> failwith msg
        | Ok l -> l)
    in
  let pb = Parse.pb sexp_l in
  let solver = Solver.create () in
  List.iter (Solver.process_stmt solver) pb;
  Format.printf "## done with file %S@." f;
  ()


let main () =
  let files = ref [] in
  let help = "usage: diamond_solver <file>" in
  Arg.parse [] (fun f->files:= f:: !files) help;
  match !files with
  | [] -> print_endline help
  | l -> List.iter process_file @@ List.rev l

let () = main()
