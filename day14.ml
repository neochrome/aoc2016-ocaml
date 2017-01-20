#use "./lib.ml";;

module Queue = struct
  include Queue;;

  let find p q =
    let exception Exit in
    let found = ref None in
    try
      Queue.iter (fun e -> if p e then (found := Some e; raise Exit)) q;
      !found
    with Exit -> !found
end

let any_seq_of length s =
  let slen = String.length s in
  let rec search i c chr =
    if c = 1 then Some chr
    else if i = slen then None
    else if s.[i] = chr then search (i + 1) (c - 1) chr
    else search (i + 1) length s.[i]
  in search 1 length s.[0]
;;

let seq_of chr length s =
  let slen = String.length s in
  let rec search = function
    | i when i = slen -> false
    | i when s.[i] <> chr -> search (i + 1)
    | i -> same 0 i
  and same n = function
    | i when i = slen -> false
    | _ when n = length -> true
    | i when s.[i] <> chr -> search (i + 1)
    | i -> same (n + 1) (i + 1)
  in search 0
;;

let rec hash s = function
  | n when n > 0 -> hash (Digest.string s |> Digest.to_hex) (n - 1)
  | _ -> s
;;
assert (hash "abc0" 0 = "abc0");;
assert (hash "abc0" 1 = Digest.to_hex (Digest.string "abc0"));;
assert (hash "abc0" 2017 = "a107ff634856bb300138cac6568c0f24");;

let hash_for rounds seed idx = hash (Printf.sprintf "%s%d" seed idx) rounds;;

type hash = { idx:int; hash:string; chr:char; fives:bool };;

let hash_and_slash hash_for idx =
  let hash = hash_for idx in
  match any_seq_of 3 hash, any_seq_of 5 hash with
  | Some chr, fives -> Some { idx; hash; chr; fives = Option.is_some fives }
  | _ -> None
;;

let generate_pad hash_for =
  let hashes = Queue.create () in
  let rec fill first_idx idx =
    if idx - first_idx >= 1000 then idx
    else
      let hash = hash_and_slash hash_for idx in
      if hash <> None then Queue.push (Option.value_of hash) hashes;
      fill first_idx (idx + 1)
  in
  let rec next first_idx idx count =
    if count = 64 then first_idx
    else
      let idx = fill first_idx idx in
      let key = Queue.pop hashes in
      let matching_hash h =
        h.fives && (seq_of key.chr 5 h.hash)
      in
      let count = match hashes |> Queue.find matching_hash with
        | None -> count
        | Some hash -> count + 1
      in next key.idx idx count
  in next 0 0 0
;;

let time lbl f =
  let start = Sys.time () in
  let res = f () in
  Sys.time () -. start |> Printf.printf "%s took: %fs\n%!" lbl;
  res
;;

let part1_hashing = hash_for 1;;
time "test1" (fun () -> generate_pad (part1_hashing "abc")) |> Printf.printf "test1: %d\n%!";;
time "part1" (fun () -> generate_pad (part1_hashing "jlmsuwbz")) |> Printf.printf "part1: %d\n%!";;

let part2_hashing = hash_for 2017;;
time "test2" (fun () -> generate_pad (part2_hashing "abc")) |> Printf.printf "test2: %d\n%!";;
time "part2" (fun () -> generate_pad (part2_hashing "jlmsuwbz")) |> Printf.printf "part2: %d\n%!";;
