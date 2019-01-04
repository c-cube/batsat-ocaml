
type t = {
  mutable id: int;
  view: view;
  mutable lit: Batsat.Lit.t option;
}

and view =
  | True
  | And of t list
  | Or of t list
  | Not of t
  | Eq of Atom.t * Atom.t

let[@inline] view t = t.view
let[@inline] hash t = CCHash.int t.id
let[@inline] equal a b = a.id = b.id

module H = Hashcons.Make(struct
    type nonrec t = t
    let equal a b : bool =
      match a.view, b.view with
      | True, True -> true
      | And l1, And l2 -> CCList.equal equal l1 l2
      | Or l1, Or l2 -> CCList.equal equal l1 l2
      | Not a, Not b -> equal a b
      | Eq (a1,a2), Eq (b1,b2) -> Atom.equal a1 b1 && Atom.equal a2 b2
      | True, _ | And _, _ | Or _, _ | Not _, _ | Eq _, _
        -> false
    let hash t = match t.view with
      | True -> CCHash.int 5
      | And l -> CCHash.combine2 10 @@ CCHash.list hash l
      | Or l -> CCHash.combine2 20 @@ CCHash.list hash l
      | Not x -> CCHash.combine2 30 @@ hash x
      | Eq (a,b) -> CCHash.combine3 40 (Atom.hash a)(Atom.hash b)

    let set_id t id =
      assert (t.id < 0);
      t.id <- id
  end)

let[@inline] make_ view =
  H.hashcons {view; id= -1; lit=None}

let true_ = make_ True

let not_ t =
  match view t with
  | Not u -> u
  | _ -> make_ (Not t)

let false_ = not_ true_

let and_ = function
  | [] -> true_
  | [x] -> x
  | l -> make_ (And l)

let or_ = function
  | [] -> false_
  | [x] -> x
  | l -> make_ (Or l)

let eq a b =
  if Atom.equal a b then true_
  else (
    (* normalize *)
    let view = if Atom.compare a b > 0 then Eq(b,a) else Eq(a,b) in
    make_ view
  )

let imply a b = or_ (b :: List.map not_ a)


let rec pp out t =
  let module Fmt = CCFormat in
  let pp_list = Fmt.(list ~sep:(return "@ ")) in
  match view t with
  | True -> Fmt.string out "true"
  | Not {view=True;_} -> Fmt.string out "false"
  | And l -> Fmt.fprintf out "(@[and@ %a@])" (pp_list pp) l
  | Or l -> Fmt.fprintf out "(@[or@ %a@])" (pp_list pp) l
  | Not u -> Fmt.fprintf out "(@[not@ %a@])" pp u
  | Eq(a,b) -> Fmt.fprintf out "(@[=@ %a@ %a@])" Atom.pp a Atom.pp b
