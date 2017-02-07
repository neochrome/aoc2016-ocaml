#use "./lib.ml";;

type tile = Trap | Safe;;
let of_char = function '^' -> Trap | '.' -> Safe | _ -> failwith "bad input";;
let to_char = function Trap -> '^' | Safe -> '.';;
let parse = String.fold (fun tiles t -> of_char t :: tiles) [];;
let to_string tiles = tiles |> List.map to_char |> String.of_list;;

let rule = function
  | Trap, Trap, Safe -> Trap
  | Safe, Trap, Trap -> Trap
  | Trap, Safe, Safe -> Trap
  | Safe, Safe, Trap -> Trap
  | _ -> Safe
;;

let next tiles =
  let rec next = function
    | [left; center; right] -> [rule (left,center,right)]
    | left :: (center :: right :: _ as tiles) -> rule (left,center,right) :: next tiles
    | _ -> failwith "invalid set of tiles"
  in next (Safe :: tiles @ [Safe])
;;

assert (parse "..^^." |> next |> next |> to_string = "^^..^");;

let expand_to times tiles =
  let rec expand tiles row = function
    | 0 -> tiles
    | n -> expand (row :: tiles) (next row) (n - 1)
  in expand [] tiles times |> List.rev
;;

let count_safe =
  List.fold_left (fun sum tiles ->
    tiles
    |> List.map (function Safe -> 1 | Trap -> 0)
    |> List.fold_left ( + ) 0
    |> ( + ) sum
  ) 0
;;

assert (parse ".^^.^.^^^^" |> expand_to 10 |> count_safe = 38);;

let input = ".^^.^^^..^.^..^.^^.^^^^.^^.^^...^..^...^^^..^^...^..^^^^^^..^.^^^..^.^^^^.^^^.^...^^^.^^.^^^.^.^^.^.";;

input
|> parse
|> expand_to 40
|> count_safe
|> Printf.printf "part1: %d\n%!";;

input
|> parse
|> expand_to 400_000
|> count_safe
|> Printf.printf "part2: %d\n%!";;
