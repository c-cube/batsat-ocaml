
type stmt =
  | SetInfo of string * string
  | SetLogic of string
  | Assert of Term.t
  | Declare_sort of Atom.t * int
  | Declare_fun of Atom.t * Atom.t list * Atom.t
  | Check_sat
  | Exit

type t = stmt list

let pp_stmt out (st:stmt) = match st with
  | SetInfo (a,b) -> Format.fprintf out "(@[set-info %s@ %s@])" a b
  | SetLogic s -> Format.fprintf out "(@[set-logic %s@])" s
  | Exit -> Format.fprintf out "(exit)"
  | Check_sat -> Format.fprintf out "(check-sat)"
  | Assert t -> Format.fprintf out "(@[assert@ %a@])" Term.pp t
  | Declare_sort (a,n) ->
    Format.fprintf out "(@[declare-sort@ %a %d@])" Atom.pp a n
  | Declare_fun (f,args,ret) ->
    Format.fprintf out "(@[declare-fun@ %a (@[%a@])@ %a@])"
      Atom.pp f (CCFormat.list Atom.pp) args Atom.pp ret
