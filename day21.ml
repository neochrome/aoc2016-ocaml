#use "./lib.ml";;

let swap_position s x y = String.mapi (fun i c -> match i with
  | i when i = x -> s.[y]
  | i when i = y -> s.[x]
  | _ -> c
) s;;
assert (swap_position "01234" 4 0 = "41230");;
assert (swap_position "01234" 0 4 = "41230");;
assert (swap_position "01234" 2 3 = "01324");;

let swap_letter s x y = String.map (function
  | c when c = x -> y
  | c when c = y -> x
  | c -> c
) s;;
assert (swap_letter "01234" '3' '1' = "03214");;

let reverse s x y =
  let x,y = min x y, max x y in
  String.mapi (fun i c -> match i with
    | i when i >= x && i <= y -> s.[y-i+x]
    | _ -> c
  ) s;;
assert (reverse "01234" 0 4 = "43210");;
assert (reverse "01234" 4 0 = "43210");;
assert (reverse "01234" 2 3 = "01324");;
assert (reverse "01234" 2 0 = "21034");;
assert (reverse "01234" 0 0 = "01234");;

let rotate_left s steps =
  let l = String.length s in
  let steps = (steps mod l + l) mod l in
  if steps = 0 then s
  else String.mapi (fun i _ -> s.[(i + steps + l) mod l]) s
;;
assert (rotate_left "01234" 1 = "12340");;
assert (rotate_left "01234" 5 = "01234");;

let rotate_right s steps = rotate_left s (-steps);;
assert (rotate_right "01234" 1 = "40123");;
assert (rotate_right "01234" 5 = "01234");;

let rotate_from_letter s x =
  let i = String.index s x in
  let steps = i + if i >= 4 then 2 else 1 in
  rotate_right s steps
;;
assert (rotate_from_letter "01234" '0' = "40123");;
assert (rotate_from_letter "01234" '1' = "34012");;
assert (rotate_from_letter "01234" '3' = "12340");;
assert (rotate_from_letter "01234" '4' = "40123");;


let move s x y =
  String.mapi (fun i c ->
    match i with
    | i when i = y -> s.[x]
    | i when (i < x && i < y) || (i > x && i > y) -> c
    | i when i <= x && i > y -> s.[i - 1]
    | i -> s.[i + 1]
  ) s
;;
assert (move "01234" 0 1 = "10234");;
assert (move "01234" 0 4 = "12340");;
assert (move "01234" 3 4 = "01243");;
assert (move "01234" 4 2 = "01423");;
assert (move "01234" 2 1 = "02134");;


let scramble input instruction =
  let try_map fmt func = function
    | None -> begin try Some (Scanf.sscanf instruction fmt (func input)) with _ -> None end
    | Some _ as result -> result
  in
  None
  |> try_map "swap position %d with position %d" swap_position
  |> try_map "swap letter %c with letter %c" swap_letter
  |> try_map "reverse positions %d through %d" reverse
  |> try_map "rotate right %d step" rotate_right
  |> try_map "rotate left %d step" rotate_left
  |> try_map "rotate based on position of letter %c" rotate_from_letter
  |> try_map "move position %d to position %d" move
  |> function
    | None -> failwith (Printf.sprintf "Invalid instruction: %s" instruction)
    | Some scrambled -> scrambled
;;

let unscramble input instruction =
  let try_map fmt func = function
    | None -> begin try Some (Scanf.sscanf instruction fmt (func input)) with _ -> None end
    | Some _ as result -> result
  in
  let rev f i a b = f i b a in
  let rev_rotate_from_letter s' x =
    let rec search = function
      | s when rotate_from_letter s x = s' -> s
      | s -> search (rotate_left s 1)
    in search (rotate_left s' 1)
  in
  None
  |> try_map "swap position %d with position %d" (rev swap_position)
  |> try_map "swap letter %c with letter %c" (rev swap_letter)
  |> try_map "reverse positions %d through %d" (rev reverse)
  |> try_map "rotate right %d step" rotate_left
  |> try_map "rotate left %d step" rotate_right
  |> try_map "rotate based on position of letter %c" rev_rotate_from_letter
  |> try_map "move position %d to position %d" (rev move)
  |> function
    | None -> failwith (Printf.sprintf "Invalid instruction: %s" instruction)
    | Some unscrambled -> unscrambled
;;

File.open_in "./day21.input" (fun ch ->
  let instructions = Stream.of_lines ch |> Stream.to_list in
  instructions |> List.fold_left scramble "abcdefgh" |> Printf.printf "part1: %s\n%!";
  instructions |> List.rev |> List.fold_left unscramble "fbgdceah" |> Printf.printf "part2: %s\n%!";
);;
