open Printf
open ExtArray

let time f x =
  let t0 = Unix.gettimeofday () in
  let y = f x in
  let dt = Unix.gettimeofday () -. t0 in
    (y, dt)

let default_iters = ref 20

let time ?(overhead = 0.) ?(iters = !default_iters) f x =
  let rec loop best = function
      n when n <= 0 -> let y, dt = time f x in (y, min (dt -. overhead) best)
    | n -> let y, dt = time f x in loop (min (dt -. overhead) best) (n - 1)
  in loop max_float iters

let shuffle a =
  let a = Array.copy a in
  let n = ref (Array.length a) in
    while !n > 1 do
      decr n;
      let k = Random.int (!n + 1) in
      let tmp = a.(k) in
        a.(k) <- a.(!n);
        a.(!n) <- tmp
    done;
    a

let sort a =
  let a = Array.copy a in
    Array.sort String.compare a;
    a

let read_lines file =
  let ic = open_in file in
  let rec loop acc =
    let line = try Some (input_line ic) with End_of_file -> None in
      match line with
          Some "" -> loop acc
        | Some line -> loop (line :: acc)
        | None -> close_in ic; Array.of_list (List.rev acc)
  in loop []

let lines = shuffle (read_lines "/usr/share/dict/american-english")
let strings = shuffle (read_lines "/usr/share/dict/american-english-large")
let strings' = shuffle (read_lines "/usr/share/dict/spanish")

let hitrate = 100. *. float (Array.length lines) /. float (Array.length strings)

let hitrate' =
  let len = Array.length lines in
  let h = Fasthashtbl.create len in
  let hits = ref 0 in
    Array.iter (fun x -> Fasthashtbl.add h x x) lines;
    Array.iter (fun x -> if Fasthashtbl.mem h x then incr hits) strings';
    100. *. float !hits /. float len

let ints =
  let len = Array.length strings in
    Array.init len (fun i -> Random.int (100 * len))

let () = ints.(0) <- 42

let timed ?overhead msg f iters =
  let y, dt = time ?overhead f iters in
    printf "  %-36s  %8.5fs  (%.0f / sec)\n%!" msg dt (float iters /. dt);
    y

let timed_array ?overhead msg f a =
  let iters = Array.length a in
  let y, dt = time ?overhead f a in
    printf "  %-36s  %8.5fs  (%.0f / sec)\n%!" msg dt (float iters /. dt);
    y

let timed_array' overhead msg f a = timed_array ~overhead msg f a

module type STRING_OPS =
sig
  type 'v t
  val id : string
  val build : string array -> string t
  val find_arr_hit : 'v t -> string array -> unit
  val find_arr_miss : 'v t -> string array -> unit
  val find_constant : 'v t -> int -> unit
  val measure_overhead : 'v t -> int -> unit
  val load_factor : 'v t -> float option
end

module type IMP_HASH =
sig
  type ('k, 'v) t
  val id : string
  val create : int -> ('k, 'v) t
  val add : ('k, 'v) t -> 'k -> 'v -> unit
  val find : ('k, 'v) t -> 'k -> 'v
  val nop : ('k, 'v) t -> int -> unit
  val load_factor : ('k, 'v) t -> float option
end

module Ops_imp (H : IMP_HASH) =
struct
  let id = H.id

  let find_arr_hit h a = Array.iter (fun l -> ignore (H.find h l)) a

  let find_arr_miss h a =
    Array.iter (fun l -> try ignore (H.find h l) with Not_found -> ()) a

  let build a =
    let h = H.create 15 in
      Array.iter (fun l -> H.add h l l) a;
      h

  let find_constant h x n = for i = 1 to n do ignore (H.find h x) done

  let measure_overhead h n = for i = 1 to n do H.nop h n done
end

module Sops_imp(H : IMP_HASH) : STRING_OPS =
struct
  type 'v t = (string, 'v) H.t
  include Ops_imp(H)
  let find_constant h n = for i = 1 to n do ignore (H.find h "test") done
  let load_factor h = H.load_factor h
end

module Sops_imp'
  (MH : functor (H : Hashtbl.HashedType with type t = string) -> sig
     type 'a t
     val id : string
     val create : int -> 'a t
     val add : 'v t -> string -> 'v -> unit
     val find : 'v t -> string -> 'v
     val nop : 'v t -> int -> unit
     val load_factor : 'v t -> float option
   end) : STRING_OPS =
struct
  module H = MH(struct
                  type t = string
                  let equal a b = 
                    String.length a = String.length b && String.compare a b = 0
                  let hash s =
                    let n = ref 0 in
                      for i = 0 to String.length s - 1 do
                        n := !n * 33 + Char.code (String.unsafe_get s i)
                      done;
                      !n
                end)
  type 'v t = 'v H.t
  let id = "Functorized " ^ H.id

  let find_arr_hit h a = Array.iter (fun l -> ignore (H.find h l)) a

  let find_arr_miss h a =
    Array.iter (fun l -> try ignore (H.find h l) with Not_found -> ()) a

  let build a =
    let h = H.create 15 in
      Array.iter (fun l -> H.add h l l) a;
      h

  let find_constant h n = for i = 1 to n do ignore (H.find h "test") done
  let measure_overhead h n = for i = 1 to n do H.nop h n done

  let load_factor = H.load_factor
end

module Sops_func(H : sig
                   type 'v t
                   val id : string
                   val empty : 'v t
                   val add : string -> 'v -> 'v t -> 'v t
                   val find : string -> 'v t -> 'v
                   val nop : 'v t -> int -> unit
                 end) : STRING_OPS with type 'v t = 'v H.t =
struct
  type 'v t = 'v H.t
  let id = H.id
  let find_arr_hit h a = Array.iter (fun l -> ignore (H.find l h)) a
  let find_arr_miss h a =
    Array.iter (fun l -> try ignore (H.find l h) with Not_found -> ()) a
  let find_constant h n = for i = 1 to n do ignore (H.find "test" h) done
  let build = Array.fold_left (fun m x -> H.add x x m) H.empty
  let measure_overhead h n = for i = 1 to n do H.nop h n done
  let load_factor h = None
end

let print_size_and_load_factor load_factor_f h =
  let lfactor_s = match load_factor_f h with
      None -> ""
    | Some f -> sprintf "(load factor %5.3f)" f
  in printf " struct size: %d %s\n" (Size.size_b h) lfactor_s

module BM_strings(Ops : STRING_OPS) =
struct
  open Ops
  let run show_size =
    printf "%s:\n" id;
    print_endline " strings";
    let sorted = sort lines in
    let rev_sorted = Array.rev sorted in
    let sorted_strings' = sort strings' in
    let shuffled = shuffle lines in
    let h = timed_array "add" build lines in
    let h' = timed_array "add (sorted)" build sorted in
    let (), o = time ~iters:10 (measure_overhead h) 20_000_000 in
    let o = o /. 2e7 in (* time per iteration *)
    let overhead a = o *. float (Array.length a) in
    let o1 = overhead lines in
    let o2 = overhead strings in
    let o3 = overhead strings' in
      if show_size then print_size_and_load_factor Ops.load_factor h;
      timed "find (hit, constant + overhead)" (find_constant h) 1000000;
      timed ~overhead:(o *. 1e6) "find (hit, constant, no overhead)"
        (find_constant h) 1_000_000;
      printf
        " overhead (%8.5fms / million items) removed from next measurements:\n"
        (o *. 1e9);
      timed_array' o1 "find (hit, randomized)" (find_arr_hit h) shuffled;
      timed_array' o1 "find (hit)" (find_arr_hit h) lines;
      timed_array' o1 "find (hit, sorted)" (find_arr_hit h) sorted;
      timed_array' o1 "find (hit, rev sorted)" (find_arr_hit h) rev_sorted;
      timed_array' o1 "find (hit, sorted vs. sorted)" (find_arr_hit h') sorted;
      timed_array' o1 "find (hit, rev sorted vs. sorted)" (find_arr_hit h')
        rev_sorted;
      timed_array' o2 (sprintf "find (%4.1f%% hit, rand)" hitrate)
        (find_arr_miss h) strings;
      timed_array' o3 (sprintf "find (%4.1f%% hit, rand)" hitrate')
        (find_arr_miss h) strings';
      timed_array' o3 (sprintf "find (%4.1f%% hit, sorted)" hitrate')
        (find_arr_miss h) sorted_strings';
      timed_array' o3 (sprintf "find (%4.1f%% hit, sorted vs. sorted)" hitrate')
        (find_arr_miss h') sorted_strings';
      print_newline ()
end

module BM_int_imp(H : IMP_HASH) =
struct
  include Ops_imp(H)

  let run show_size =
    printf "%s:\n" H.id;
    print_endline " ints";
    let h =
      timed_array "add"
        (fun a -> let h = H.create 13 in Array.iter (fun l -> H.add h l l) a; h)
        ints in 
    let (), o = time ~iters:10 (measure_overhead h) 20_000_000 in
    let o = o /. 2e7 in (* time per iteration *)
    let o1 = o *. float (Array.length ints) in
      if show_size then print_size_and_load_factor H.load_factor h;
      timed "find (constant w/ overhead)" (find_constant h 42) 1000000;
      timed ~overhead:(o *. 1e6) "find (constant, no overhead)"
        (find_constant h 42) 1000000;
      printf 
        " overhead (%8.5fms / million items) removed from next measurements:\n"
        (o *. 1e9);
      timed_array' o1 "find (hit, randomized)" (find_arr_hit h) (shuffle ints);
      timed_array' o1 "find (hit)" (find_arr_hit h) (ints);
      timed_array' o1 "find (1% hit, rand.)"
        (find_arr_miss h)
        (shuffle (Array.init (Array.length ints) (fun i -> i)));
      print_newline ()
end

let nop h n = ()

module FH = struct 
  include Fasthashtbl 
  let id = "Fasthashtbl" 
  let nop = nop 
  let load_factor h = Some (load_factor h)
end

module H = struct
  include Hashtbl 
  let id = "Hashtbl"
  let nop = nop 
  let load_factor h = None
end

module H' = struct 
  include Hashtbl_mod 
  let id = "Hashtbl_mod" 
  let nop = nop 
  let load_factor h = Some (load_factor h)
end

module H'' = struct 
  include Hashtbl_hval 
  let id = "Hashtbl_hval" 
  let nop = nop 
  let load_factor h = Some (load_factor h)
end

module T = struct include Ternary let id = "Ternary" let nop = nop end

module TM = struct include Trie_map let id = "Trie_map" let nop = nop end

module TM' = struct include Trie_map_mod let id = "Trie_map'" let nop = nop end

module M = struct include Map.Make(String) let id = "Map" let nop = nop end


module type HT = Hashtbl.HashedType

module MH(H : HT) = struct
  include Hashtbl.Make(H)
  let id = "Hashtbl"
  let nop = nop
  let load_factor h = None
end

module MH'(H : HT) = struct 
  include Hashtbl_mod.Make(H) 
  let id = "Hashtbl_mod" 
  let nop = nop 
  let load_factor h = None
end

module MH''(H : HT) = struct 
  include Hashtbl_hval.Make(H) 
  let id = "Hashtbl_hval" 
  let nop = nop 
  let load_factor h = None
end

let show_sizes = ref false

let args = [
  "-n", Arg.Set_int default_iters, "N Number of iterations (default: 10)";
  "-s", Arg.Set show_sizes, " Show structure sizes.";
]

let () =
  Arg.parse (Arg.align args) ignore "";
  printf "String set size: %d\n" (Array.length lines);
  printf "Target array 1: %d\n" (Array.length strings);
  printf "Target array 2: %d\n" (Array.length strings');
  print_newline ();
  Gc.compact ();
  for i = 0 to 0 do
    let module B = BM_int_imp(FH) in B.run !show_sizes;
    let module B = BM_int_imp(H) in B.run !show_sizes;
    let module B = BM_int_imp(H') in B.run !show_sizes;
    let module B = BM_int_imp(H'') in B.run !show_sizes;
    let module B = BM_strings(Sops_imp(FH)) in B.run !show_sizes;
    let module B = BM_strings(Sops_imp(H)) in B.run !show_sizes;
    let module B = BM_strings(Sops_imp(H')) in B.run !show_sizes;
    let module B = BM_strings(Sops_imp(H'')) in B.run !show_sizes;

    (* let module B = BM_strings(Sops_imp'(MH)) in B.run !show_sizes; *)
    (* let module B = BM_strings(Sops_imp'(MH')) in B.run !show_sizes; *)
    (* let module B = BM_strings(Sops_imp'(MH'')) in B.run !show_sizes; *)

    let module B = BM_strings(Sops_func(T)) in B.run !show_sizes;
    let module B = BM_strings(Sops_func(TM)) in B.run false;
    let module B = BM_strings(Sops_func(TM')) in B.run !show_sizes;
    let module B = BM_strings(Sops_func(M)) in B.run !show_sizes;
      ()
  done
