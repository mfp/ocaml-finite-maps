type 'a t = E
          | L of 'a
          | B of      char * 'a t * 'a t * 'a t
          | K of 'a * char * 'a t * 'a t * 'a t

let empty = E

let lookup k t =
  let n = String.length k in
  let rec go i = function
  | E                             -> None
  | L  v              when i == n -> Some v
  | L  _                          -> None
  | B (   _, _, _, _) when i == n -> None
  | B (   c, l, q, h)             ->
    let c' = k.[i] in
    if  c' < c then go  i    l else
    if  c' > c then go  i    h else
                    go (i+1) q
  | K (v, _, _, _, _) when i == n -> Some v
  | K (_, c, l, q, h)             ->
    let c' = k.[i] in
    if  c' < c then go  i    l else
    if  c' > c then go  i    h else
                    go (i+1) q
  in go 0 t

let add k v t =
  let n = String.length k in
  let rec go i = function
  | E                 when i == n -> L  v
  | E                             -> B (   k.[i], E, go (i+1) E, E)
  | L  _              when i == n -> L  v
  | L  v                          -> K (v, k.[i], E, go (i+1) E, E)
  | B (   c, l, q, h) when i == n -> K (v, c    , l, q         , h)
  | B (   c, l, q, h)             ->
    let c' = k.[i] in
    if  c' < c then B (   c, go i l,          q,      h) else
    if  c' > c then B (   c,      l,          q, go i h) else
                    B (   c,      l, go (i+1) q,      h)
  | K (_, c, l, q, h) when i == n -> K (v, c    , l, q         , h)
  | K (v, c, l, q, h)             ->
    let c' = k.[i] in
    if  c' < c then K (v, c, go i l,          q,      h) else
    if  c' > c then K (v, c,      l,          q, go i h) else
                    K (v, c,      l, go (i+1) q,      h)
  in go 0 t

let remove k t =
  let prune = function
  | B (   _, E, E, E) -> E
  | K (v, _, E, E, E) -> L v
  | t                 -> t
  in
  let n = String.length k in
  let rec go i t = match t with
  | E                             -> t
  | L  _              when i == n -> E
  | L  _                          -> t
  | B (   _, _, _, _) when i == n -> t
  | B (   c, l, q, h)             ->
    let c' = k.[i] in
    if  c' < c then prune (B (   c, go i l,          q,      h)) else
    if  c' > c then prune (B (   c,      l,          q, go i h)) else
                    prune (B (   c,      l, go (i+1) q,      h))
  | K (_, c, l, q, h) when i == n -> B (c, l, q, h)
  | K (v, c, l, q, h) ->
    let c' = k.[i] in
    if  c' < c then prune (K (v, c, go i l,          q,      h)) else
    if  c' > c then prune (K (v, c,      l,          q, go i h)) else
                    prune (K (v, c,      l, go (i+1) q,      h))
  in go 0 t

let find k t = match lookup k t with
| Some x -> x
| None   -> raise Not_found

let mem k t = match lookup k t with
| Some _ -> true
| None   -> false

