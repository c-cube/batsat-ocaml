
type t = {
  mutable id: int;
  str: string;
}

module H = Hashcons.Make(struct
    type nonrec t = t
    let equal a b = String.equal a.str b.str
    let hash a = Hashtbl.hash a.str
    let set_id x id =
      assert (x.id < 0);
      x.id <- id
      end)

let[@inline] make s : t =
  H.hashcons {id= -1; str=s }


let[@inline] equal a b : bool = a.id = b.id
let[@inline] compare a b : int = Pervasives.compare a.id b.id
let[@inline] hash a : int = CCHash.int a.id
let pp out a = CCFormat.string out a.str
