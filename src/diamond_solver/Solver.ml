
(* FIXME *)
type t = unit

let create() : t = ()

let process_stmt (s:t) (stmt: Pb.stmt) : unit =
  Format.printf "(@[<1>process@ %a@])@." Pb.pp_stmt stmt;
  match stmt with
  | Pb.Declare_fun _ | Pb.Declare_sort _ | Pb.SetInfo _ | Pb.SetLogic _ -> ()
  | Pb.Assert t ->
    assert false (* TODO *)
  | Pb.Check_sat ->
    assert false (* TODO *)
  | Pb.Exit -> ()
