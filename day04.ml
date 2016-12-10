#use "./lib.ml";;

let checksum str =
  let rec count freqs = function
    | 0 -> freqs
    | i ->
      begin match str.[i - 1] with
      | '-' -> count freqs (i - 1)
      | c ->
        try
          let f = List.assoc c freqs in
          count ((c , f + 1) :: (freqs |> List.remove_assoc c)) (i - 1)
        with Not_found -> count ((c, 1) :: freqs) (i - 1)
      end
  in
  let freqs =
    count [] (String.length str)
    |> List.sort (fun (ac,af) (bc,bf) ->
      match compare bf af with
      | 0 -> compare ac bc
      | c -> c
    )
    |> List.map fst
  in
  String.init 5 (List.nth freqs)
;;

let verify room =
  Scanf.sscanf room "%[-a-z]%u[%[a-z]]" (fun enc id chk ->
    let enc = String.sub enc 0 (String.length enc - 1) in
    if checksum enc = chk then Some (id,enc) else None
  )
;;

let part1 stream =
  stream
  |> Stream.map verify
  |> Stream.filter Option.is_some
  |> Stream.map Option.value_of
  |> Stream.map fst
  |> Stream.reduce ( + ) 0
;;

assert ([
  "aaaaa-bbb-z-y-x-123[abxyz]";
  "a-b-c-d-e-f-g-h-987[abcde]";
  "not-a-real-room-404[oarel]";
  "totally-real-room-200[decoy]";
] |> Stream.of_list |> part1 = 1514);;

File.open_in "./day04.input" (fun ch ->
  Stream.of_lines ch
  |> part1
  |> Printf.printf "par1: %d\n"
);;

let rofl n = function
  | '-' -> ' '
  | c -> 97 + (Char.code c - 97 + n) mod 26 |> Char.chr
;;
assert ("qzmt-zixmtkozy-ivhz" |> String.map (rofl 343) = "very encrypted name");;

File.open_in "./day04.input" (fun ch ->
  Stream.of_lines ch
  |> Stream.map verify
  |> Stream.filter Option.is_some
  |> Stream.map Option.value_of
  |> Stream.map (fun (id,enc) -> id, enc |> String.map (rofl id))
  |> Stream.find (fun (id,name) -> name = "northpole object storage")
  |> fst
  |> Printf.printf "part2: %d\n"
);;
