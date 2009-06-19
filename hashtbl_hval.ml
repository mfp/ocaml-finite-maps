(***********************************************************************)
(*                                                                     *)
(*                           Objective Caml                            *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* $Id: hashtbl.ml,v 1.27 2005-10-25 18:34:07 doligez Exp $ *)

(* Hash tables *)

external hash_param : int -> int -> 'a -> int = "caml_hash_univ_param" "noalloc"

let hash x = hash_param 10 100 x

(* We do dynamic hashing, and resize the table and rehash the elements
   when buckets become too long. *)

type ('a, 'b) t =
  { mutable size: int;                        (* number of elements *)
    mutable data: ('a, 'b) bucketlist array } (* the buckets *)

and ('a, 'b) bucketlist =
    Empty
  | Cons of int * 'a * 'b * ('a, 'b) bucketlist

let create initial_size =
  let s = min (max 1 initial_size) Sys.max_array_length in
  { size = 0; data = Array.make s Empty }

let clear h =
  for i = 0 to Array.length h.data - 1 do
    h.data.(i) <- Empty
  done;
  h.size <- 0

let copy h =
  { size = h.size;
    data = Array.copy h.data }

let length h = h.size

let load_factor h = float h.size /. float (Array.length h.data)

let resize hashfun tbl =
  let odata = tbl.data in
  let osize = Array.length odata in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if nsize <> osize then begin
    let ndata = Array.create nsize Empty in
    let rec insert_bucket = function
        Empty -> ()
      | Cons(hval, key, data, rest) ->
          insert_bucket rest; (* preserve original order of elements *)
          let nidx = hval mod nsize in
          ndata.(nidx) <- Cons(hval, key, data, ndata.(nidx)) in
    for i = 0 to osize - 1 do
      insert_bucket odata.(i)
    done;
    tbl.data <- ndata;
  end

let add h key info =
  let hval = hash key in
  let i = hval mod (Array.length h.data) in
  let bucket = Cons(hval, key, info, h.data.(i)) in
  h.data.(i) <- bucket;
  h.size <- succ h.size;
  if h.size > Array.length h.data lsr 1 then resize hash h

let remove h key =
  let rec remove_bucket hval = function
      Empty ->
        Empty
    | Cons(hval', k, i, next) ->
        if hval = hval' && compare k key = 0
        then begin h.size <- pred h.size; next end
        else Cons(hval', k, i, remove_bucket hval next) in
  let hval = hash key in
  let i = hval mod (Array.length h.data) in
  h.data.(i) <- remove_bucket hval h.data.(i)

let rec find_rec hval key = function
    Empty ->
      raise Not_found
  | Cons(hval', k, d, rest) ->
      if hval = hval' && compare key k = 0 then d else find_rec hval key rest

let find h key =
  let hval = hash key in
  match h.data.(hval mod (Array.length h.data)) with
    Empty -> raise Not_found
  | Cons(hval', k1, d1, rest1) ->
      if hval = hval' && compare key k1 = 0 then d1 else
      match rest1 with
        Empty -> raise Not_found
      | Cons(hval', k2, d2, rest2) ->
          if hval = hval' && compare key k2 = 0 then d2 else
          match rest2 with
            Empty -> raise Not_found
          | Cons(hval', k3, d3, rest3) ->
              if hval = hval' && compare key k3 = 0 then d3 else find_rec hval key rest3

let find_all h key =
  let rec find_in_bucket hval = function
    Empty ->
      []
  | Cons(hval', k, d, rest) ->
      if hval = hval' && compare k key = 0
      then d :: find_in_bucket hval rest
      else find_in_bucket hval rest in
  let hval = hash key in
  find_in_bucket hval h.data.(hval mod (Array.length h.data))

let replace h key info =
  let rec replace_bucket hval = function
      Empty ->
        raise Not_found
    | Cons(hval', k, i, next) ->
        if hval = hval' && compare k key = 0
        then Cons(hval', k, info, next)
        else Cons(hval', k, i, replace_bucket hval next) in
  let hval = hash key in
  let i = hval mod (Array.length h.data) in
  let l = h.data.(i) in
  try
    h.data.(i) <- replace_bucket hval l
  with Not_found ->
    h.data.(i) <- Cons(hval, key, info, l);
    h.size <- succ h.size;
    if h.size > Array.length h.data lsl 1 then resize hash h

let mem h key =
  let rec mem_in_bucket hval = function
  | Empty ->
      false
  | Cons(hval', k, d, rest) ->
      hval = hval' && compare k key = 0 || mem_in_bucket hval rest in
  let hval = hash key in
  mem_in_bucket hval h.data.(hval mod (Array.length h.data))

let iter f h =
  let rec do_bucket = function
      Empty ->
        ()
    | Cons(_, k, d, rest) ->
        f k d; do_bucket rest in
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    do_bucket d.(i)
  done

let fold f h init =
  let rec do_bucket b accu =
    match b with
      Empty ->
        accu
    | Cons(_, k, d, rest) ->
        do_bucket rest (f k d accu) in
  let d = h.data in
  let accu = ref init in
  for i = 0 to Array.length d - 1 do
    accu := do_bucket d.(i) !accu
  done;
  !accu

(* Functorial interface *)

module type HashedType =
  sig
    type t
    val equal: t -> t -> bool
    val hash: t -> int
  end

module type S =
  sig
    type key
    type 'a t
    val create: int -> 'a t
    val clear: 'a t -> unit
    val copy: 'a t -> 'a t
    val add: 'a t -> key -> 'a -> unit
    val remove: 'a t -> key -> unit
    val find: 'a t -> key -> 'a
    val find_all: 'a t -> key -> 'a list
    val replace : 'a t -> key -> 'a -> unit
    val mem : 'a t -> key -> bool
    val iter: (key -> 'a -> unit) -> 'a t -> unit
    val fold: (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
    val length: 'a t -> int
  end

module Make(H: HashedType): (S with type key = H.t) =
  struct
    type key = H.t
    type 'a hashtbl = (key, 'a) t
    type 'a t = 'a hashtbl
    let create = create
    let clear = clear
    let copy = copy

    let safehash key = (H.hash key) land max_int

    let add h key info =
      let hval = safehash key in
      let i = hval mod (Array.length h.data) in
      let bucket = Cons(hval, key, info, h.data.(i)) in
      h.data.(i) <- bucket;
      h.size <- succ h.size;
      if h.size > Array.length h.data lsr 1 then resize safehash h

    let remove h key =
      let rec remove_bucket hval = function
          Empty ->
            Empty
        | Cons(hval', k, i, next) ->
            if hval = hval' && H.equal k key
            then begin h.size <- pred h.size; next end
            else Cons(hval', k, i, remove_bucket hval next) in
      let hval = safehash key in
      let i = hval mod (Array.length h.data) in
      h.data.(i) <- remove_bucket hval h.data.(i)

    let rec find_rec hval key = function
        Empty ->
          raise Not_found
      | Cons(hval', k, d, rest) ->
          if hval = hval' && H.equal key k then d else find_rec hval key rest

    let find h key =
      let hval = safehash key in
      match h.data.(hval mod (Array.length h.data)) with
        Empty -> raise Not_found
      | Cons(hval', k1, d1, rest1) ->
          if hval' = hval && H.equal key k1 then d1 else
          match rest1 with
            Empty -> raise Not_found
          | Cons(hval', k2, d2, rest2) ->
              if hval' = hval && H.equal key k2 then d2 else
              match rest2 with
                Empty -> raise Not_found
              | Cons(hval', k3, d3, rest3) ->
                  if hval' = hval && H.equal key k3 then d3 else find_rec hval key rest3

    let find_all h key =
      let rec find_in_bucket hval = function
        Empty ->
          []
      | Cons(hval', k, d, rest) ->
          if hval = hval' && H.equal k key
          then d :: find_in_bucket hval rest
          else find_in_bucket hval rest in
      let hval = safehash key in
      find_in_bucket hval h.data.(hval mod (Array.length h.data))

    let replace h key info =
      let rec replace_bucket hval = function
          Empty ->
            raise Not_found
        | Cons(hval', k, i, next) ->
            if hval = hval' && H.equal k key
            then Cons(hval', k, info, next)
            else Cons(hval', k, i, replace_bucket hval next) in
      let hval = safehash key in
      let i = hval mod (Array.length h.data) in
      let l = h.data.(i) in
      try
        h.data.(i) <- replace_bucket hval l
      with Not_found ->
        h.data.(i) <- Cons(hval, key, info, l);
        h.size <- succ h.size;
        if h.size > Array.length h.data lsl 1 then resize safehash h

    let mem h key =
      let rec mem_in_bucket hval = function
      | Empty ->
          false
      | Cons(hval', k, d, rest) ->
          hval = hval' && H.equal k key || mem_in_bucket hval rest in
      let hval = safehash key in
      mem_in_bucket hval h.data.(hval mod (Array.length h.data))

    let iter = iter
    let fold = fold
    let length = length
  end
