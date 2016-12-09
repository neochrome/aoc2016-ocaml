#use "./lib.ml";;
#load "str.cma";;

type instruction =
  | Right of int
  | Left of int
  | Ahead of int
;;

let parse instruction = Scanf.sscanf instruction "%c%d" (fun dir steps ->
  if dir = 'R' then Right steps else Left steps
);;
assert (parse "R123" = Right 123);;

type position = { x: int; y: int };;
let origin = { x=0; y=0 };;
let distance pos = (abs pos.x) + (abs pos.y);;
type direction = North | East | South | West;;

let turn_right = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North
;;

let turn_left = function
  | North -> West
  | West -> South
  | South -> East
  | East -> North
;;

let step pos = function
  | North -> { pos with y=pos.y+1 }
  | East ->  { pos with x=pos.x+1 }
  | South -> { pos with y=pos.y-1 }
  | West ->  { pos with x=pos.x-1 }
;;

module Locations = Set.Make(struct
  type t = position;;
  let compare p1 p2 =
    match Pervasives.compare p1.x p2.x with
    | 0 -> Pervasives.compare p1.y p2.y
    | c -> c
  ;;
end);;
let visited = Locations.mem;;
let visit = Locations.add;;

let rec walk instruction (pos, dir, locations, part2) =
  let locations', part2' = match part2 with
  | Some _ -> locations, part2
  | None when locations |> visited pos -> locations, Some pos
  | None -> locations |> visit pos, None
  in
  match instruction with
  | Ahead 0 -> (pos, dir, locations, part2)
  | Ahead steps -> walk (Ahead (steps - 1)) (step pos dir, dir, locations', part2')
  | Right steps -> let dir' = turn_right dir in
    walk (Ahead (steps - 1)) (step pos dir', dir', locations', part2')
  | Left  steps -> let dir' = turn_left dir in
    walk (Ahead (steps - 1)) (step pos dir', dir', locations', part2')
;;

File.open_in "day01.input" (fun ch ->
  Stream.of_lines ch
  |> Stream.chunk (fun line -> Str.split (Str.regexp "[, ]+") line)
  |> Stream.map parse
  |> Stream.fold walk (origin, North, Locations.empty, None)
  |> (fun (pos, _, _, part2) ->
    Printf.printf "part1: hq found at x: %d, y: %d, distance: %d\n" pos.x pos.y (distance pos);
    match part2 with
    | None     -> Printf.printf "part2: no hq found!\n";
    | Some pos -> Printf.printf "part2: hq found at x: %d, y: %d, distance: %d\n" pos.x pos.y (distance pos);
  )
);;
