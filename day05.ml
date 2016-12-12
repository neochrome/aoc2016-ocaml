#use "./lib.ml";;
let numbers i = Some i;;

let hash id i =
  Printf.sprintf "%s%d" id i
  |> Digest.string
  |> Digest.to_hex
;;

let interesting hash =
  let rec check = function
    | 5 -> true
    | n -> if hash.[n] != '0' then false else check (n + 1)
  in check 0
;;

let pwd_char1 hash = hash.[5];;

let pwd_char2 hash =
  let ctoi = String.index "0123456789abcdef" in
  let idx = ctoi hash.[5] in
  if idx > 7 then None
  else Some (idx, hash.[6])
;;

let examples = [
  3231929, '1', Some (1, '5');
  5017308, '8', None;
  5278568, 'f', None;
  5357525, '4', Some (4, 'e');
];;
examples
|> List.iter (fun (n, expected_char, expected_char_pos) ->
  let h = hash "abc" n in
  assert (h |> interesting = true);
  assert (h |> pwd_char1 = expected_char);
  assert (h |> pwd_char2 = expected_char_pos);
);;


module PwdChars = Set.Make(struct
  type t = int * char
  let compare (c1,_) (c2,_) = Pervasives.compare c1 c2
end);;

Printf.printf "part1: %!";
Stream.from numbers
|> Stream.map (hash "cxdnnyjw")
|> Stream.filter interesting
|> Stream.mapi (fun i hash -> if i < 8 then Printf.printf "%c%!" (pwd_char1 hash); hash)
|> Stream.map pwd_char2
|> Stream.filter Option.is_some
|> Stream.map Option.value_of
|> Stream.fold_while (fun c chars ->
  if PwdChars.cardinal chars = 8 then None
  else if PwdChars.mem c chars then Some chars
  else Some (PwdChars.add c chars)
) PwdChars.empty
|> PwdChars.fold (fun (p,c) chars ->
    chars.(p) <- c;
    chars
) (Array.make 8 '_')
|> (fun chars ->
  Printf.printf "\npart2: "; chars |> Array.iter print_char; Printf.printf "\n";
);;
