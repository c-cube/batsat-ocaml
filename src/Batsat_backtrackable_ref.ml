
module Vec = struct
  type 'a t = {
    mutable data : 'a array;
    mutable sz : int;
  }

  let[@inline] create () = {data = [||]; sz = 0}

  let[@inline] is_full t = Array.length t.data = t.sz

  let[@inline] get t i =
    if i < 0 || i >= t.sz then invalid_arg "vec.get";
    Array.unsafe_get t.data i

  let[@inline] size t = t.sz

  let[@inline] shrink t i =
    assert (i >= 0);
    assert (i<=t.sz);
    t.sz <- i

  (* grow the array *)
  let[@inline never] grow_to_double_size t x : unit =
    if Array.length t.data = Sys.max_array_length then (
      failwith "vec: cannot resize";
    );
    let size =
      min Sys.max_array_length (max 4 (2 * Array.length t.data))
    in
    let arr' = Array.make size x in
    Array.blit t.data 0 arr' 0 (Array.length t.data);
    t.data <- arr';
    assert (Array.length t.data > t.sz);
    ()

  let[@inline] push t x : unit =
    if is_full t then grow_to_double_size t x;
    Array.unsafe_set t.data t.sz x;
    t.sz <- t.sz + 1
end

type 'a t = {
  mutable cur: 'a;
  stack: 'a Vec.t;
  copy: ('a -> 'a) option;
}

let create ?copy x: _ t =
  {cur=x; stack=Vec.create(); copy}

let[@inline] get self = self.cur
let[@inline] set self x = self.cur <- x
let[@inline] update self f = self.cur <- f self.cur

let[@inline] n_levels self = Vec.size self.stack

let[@inline] push_level self : unit =
  let x = self.cur in
  let x = match self.copy with None -> x | Some f -> f x in
  Vec.push self.stack x

let pop_levels self n : unit =
  assert (n>=0);
  if n > Vec.size self.stack then invalid_arg "Backtrackable_ref.pop_levels";
  let i = Vec.size self.stack-n in
  let x = Vec.get self.stack i in
  self.cur <- x;
  Vec.shrink self.stack i;
  ()
