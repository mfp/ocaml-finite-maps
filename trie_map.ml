
(* code by Matías Giovannini, retrieved from
 * http://alaska-kamtchatka.blogspot.com/2009/06/simple-efficient-trie-maps.html *)

type key = string
type 'a t = 'a node option
and 'a node =
{ value : 'a option;
  split : char;
  lokid : 'a t;
  eqkid : 'a t;
  hikid : 'a t; }

let empty = None

let is_empty = function None -> true | Some _ -> false


let lookup k t =
  let n = String.length k in
  let rec go i = function
  | None   -> None
  | Some t ->
    let cmp = Char.compare k.[i] t.split in
    if cmp < 0 then go  i    t.lokid else
    if cmp > 0 then go  i    t.hikid else
    if i+1 < n then go (i+1) t.eqkid else
                    t.value
  in go 0 t

let find k t = match lookup k t with
| Some x -> x
| None   -> raise Not_found

let mem k t = match lookup k t with
| Some _ -> true
| None   -> false

let add k v t =
  let n = String.length k in
  let rec go i = function
  | None when i+1 < n ->
    Some { value = None  ; split = k.[i]; lokid = None; eqkid = go (i+1) None; hikid = None }
  | None              ->
    Some { value = Some v; split = k.[i]; lokid = None; eqkid =          None; hikid = None }
  | Some t            ->
    let cmp = Char.compare k.[i] t.split in
    if cmp < 0 then Some { t with lokid = go  i    t.lokid } else
    if cmp > 0 then Some { t with hikid = go  i    t.hikid } else
    if i+1 < n then Some { t with eqkid = go (i+1) t.eqkid } else
                    Some { t with value = Some v }
  in go 0 t

let remove k t =
  let prune = function
  | { value = None; lokid = None; eqkid = None; hikid = None } -> None
  | t -> Some t
  in
  let n = String.length k in
  let rec go i = function
  | None   -> None
  | Some t ->
    let cmp = Char.compare k.[i] t.split in
    if cmp < 0 then prune { t with lokid = go  i    t.lokid } else
    if cmp > 0 then prune { t with hikid = go  i    t.hikid } else
    if i+1 < n then prune { t with eqkid = go (i+1) t.eqkid } else
                    prune { t with value = None             }
  in go 0 t

let fold f t e =
  let rec go prefix e = function
  | None   -> e
  | Some t ->
    let e   = go prefix e t.lokid in
    let key = prefix ^ String.make 1 t.split in
    let e   = match t.value with None -> e | Some v -> f key v e in
    let e   = go key    e t.eqkid in
              go prefix e t.hikid
  in go "" e t

let iter f t = fold (fun k v () ->        f k v ) t ()
and map  f t = fold (fun k v    -> add k (f v  )) t empty
and mapi f t = fold (fun k v    -> add k (f k v)) t empty

let compare _ _ _ = failwith "Not comparable"

let equal eq t t' =
  0 == compare (fun v w -> if eq v w then 0 else 1) t t'
