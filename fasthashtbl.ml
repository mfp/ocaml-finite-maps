
type ('k, 'v) t = 
    { mutable size : int; 
      mutable deletions : int;
      mutable data : ('k, 'v) entry array }

and ('k, 'v) entry = Empty | Removed | Data of 'k * 'v * int

(* max_array_length is odd, want an even factor *)
let max_len = (Sys.max_array_length + 1) / 2

let hash = Hashtbl.hash

let pow_of_two_size n =
  let rec loop m = 
    if m > max_len then max_len
    else if m >= n then m
    else loop (2 * m)
  in loop 8

let mask h = Array.length h.data - 1

let length h = h.size

let stride hval mask = (((hval lsr 16) lor (hval lsl 16)) land mask) lor 1

let max_deletions h = Array.length h.data lsr 1

let create initial_size =
  let s = pow_of_two_size initial_size in
    { size = 0; deletions = 0; data = Array.make s Empty }

let resize h nsize =
  let odata = h.data in
  let nsize = pow_of_two_size nsize in
  let osize = Array.length odata in
    if nsize <> osize then begin
      let ndata = Array.create nsize Empty in
      let nmask = nsize - 1 in
        for i = 0 to osize - 1 do
          match odata.(i) with
              Empty -> ()
            | Removed -> ()
            | Data (_, _, hash) as data ->
                let pos = hash land nmask in
                  match ndata.(pos) with
                      Empty -> ndata.(pos) <- data
                    | Removed -> assert false
                    | Data _ ->
                        let stride = stride hash nmask in
                        let rec attempt pos = 
                          match ndata.(pos) with
                            Empty -> ndata.(pos) <- data
                          | Removed -> assert false (* no removed at first *)
                          | Data _ -> attempt ((pos + stride) land nmask)
                        in attempt ((pos + stride) land nmask)
        done;
        h.data <- ndata;
        h.deletions <- 0
    end

let add h k v =
  let () =
    let osize = Array.length h.data in
      if h.size + 1 > osize lsr 1 then resize h (osize * 2) in
  let mask = mask h in
  let hval = hash k in
  let i = hval land mask in
    h.size <- h.size + 1;
    match h.data.(i) with
        Empty -> h.data.(i) <- Data (k, v, hval)
      | Removed -> h.deletions <- h.deletions - 1;
                   h.data.(i) <- Data (k, v, hval)
      | Data (k', _, hval') when hval = hval' && k = k' ->
          h.data.(i) <- Data (k, v, hval)
      | Data (k', v', hval') ->
          let m = stride hval mask in
          let rec walk_and_add n = 
            let n = (n + m) land mask in
              match h.data.(n) with
                  Empty -> h.data.(n) <- Data (k, v, hval)
                | Removed -> h.deletions <- h.deletions - 1;
                             h.data.(n) <- Data (k, v, hval)
                | Data (k', _, hval') when hval = hval' && k = k' ->
                    h.data.(n) <- Data (k, v, hval)
                | _ -> walk_and_add n
          in walk_and_add i

let resize_after_remove h =
  if h.deletions > max_deletions h then
    resize h (pow_of_two_size (2 * h.size))

let remove h k =
  let mask = mask h in
  let hval = hash k in
  let i = hval land mask in
    match h.data.(i) with
        Empty -> ()
      | Data (k', _, hval') when hval = hval' && k = k' ->
          h.data.(i) <- Removed;
          h.deletions <- h.deletions + 1;
          h.size <- h.size - 1;
          resize_after_remove h
      | _ -> 
          let stride = stride hval mask in
          let rec walk_and_remove n =
            let n = (n + stride) land mask in
              match h.data.(n) with
                  Empty -> ()
                | Data (k', _, hval') when hval = hval' && k = k' ->
                    h.data.(n) <- Removed;
                    h.deletions <- h.deletions + 1;
                    h.size <- h.size - 1
                | _ -> walk_and_remove n
          in walk_and_remove i;
             resize_after_remove h

let find h k =
  let mask = mask h in
  let hval = hash k in
  let i = hval land mask in
    match h.data.(i) with
        Empty -> raise Not_found
      | Data (k', v, hval') when hval = hval' && k = k' -> v
      | _ ->
          let rec walk data n stride mask = 
            let n = (n + stride) land mask in 
              match data.(n) with
                  Empty -> raise Not_found
                | Data (k', v, hval') when hval = hval' && k = k' -> v
                | _ -> walk data n stride mask
          in walk h.data i (stride hval mask) mask

let mem h k = try ignore (find h k); true with Not_found -> false

let load_factor h = float h.size /. float (Array.length h.data)

let del_factor h = float h.deletions /. float (Array.length h.data)
