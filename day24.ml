#use "./lib.ml";;

type tile =
  | Wall
  | Destination of char * int * int
  | Floor
;;
type pos = int * int;;

let parse_line y line =
  let rec parse row = function
    | -1 -> row
    | x ->
      let t = match line.[x] with
        | '#' -> Wall
        | '.' -> Floor
        | d -> Destination (d,x,y)
      in parse (t :: row) (x - 1)
  in parse [] (String.length line - 1)
  |> Array.of_list
;;
let parse_all lines =
  let map = lines |> Stream.mapi parse_line |> Stream.to_array in
  map |> Array.fold_left (fun dests row ->
    row |> Array.fold_left (fun dests t ->
      match t with
      | Destination (d,x,y) -> (d,x,y) :: dests
      | _ -> dests
    ) dests
  ) []
  |> List.sort (fun (a,_,_) (b,_,_) -> compare a b)
  |> List.map (fun (_,x,y) -> (x,y))
  , map
;;

let solve (destinations, map) is_final =
  let module BFSMap = BFS.Make(struct
    type t = pos * pos list;;
    type 'a hash = pos * pos list;;
    let hash state = state;;
    let is_final = is_final;;
    let possible_from ((x,y), left) =
      [x,y-1; x,y+1; x-1,y; x+1,y]
      |> List.map (fun (x,y) -> match map.(y).(x) with
        | Wall -> None
        | Floor -> Some ((x,y), left)
        | Destination _ -> Some ((x,y), left |> List.except (x,y))
      )
      |> List.keep Option.is_some
      |> List.map Option.value_of
    ;;
  end) in
  BFSMap.search (List.hd destinations, List.tl destinations)
;;

let part1 (destinations, map) =
  solve (destinations, map) (fun (_, left) -> left = [])
  |> function
    | None -> failwith "no solution found"
    | Some (steps, _) -> steps
;;

let part2 (destinations, map) =
  let start = List.hd destinations in
  solve (destinations, map) (function
    | p,[] when p = start -> true
    | _ -> false
  ) |> function
    | None -> failwith "no solution found"
    | Some (steps, _) -> steps
;;

[
  "###########";
  "#0.1.....2#";
  "#.#######.#";
  "#4.......3#";
  "###########";
] |> Stream.of_list
|> parse_all
|> part1
|> fun steps -> assert (steps = 14);;

[
  "###########";
  "#0.1.....2#";
  "#.#######.#";
  "#4.......3#";
  "###########";
] |> Stream.of_list
|> parse_all
|> part2
|> fun steps -> assert (steps = 20);;

File.open_in "./day24.input" (fun ch ->
  let input = Stream.of_lines ch |> parse_all in
  part1 input |> Printf.printf "part1: %d\n%!";
  part2 input |> Printf.printf "part2: %d\n%!";
);;
