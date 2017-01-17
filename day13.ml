#use "./lib.ml";;

let rec count_ones bits v =
  if bits = 0 then 0
  else v land 1 + count_ones (bits - 1) (v lsr 1)
;;

let wall_at num x y =
  x*x + 3*x + 2*x*y + y + y*y + num
  |> count_ones 32
  |> (fun ones -> ones mod 2 <> 0)
;;

let verify y expected =
  let actual =
    Seq.range 0 9
    |> Seq.to_list
    |> List.map (fun x -> wall_at 10 x y)
    |> List.map (function false -> '.' | true -> '#')
    |> String.from_list
  in if actual <> expected then
    failwith (Printf.sprintf "y: %d expected '%s', got '%s'" y expected actual)
;;

[
  ".#.####.##";
  "..#..#...#";
  "#....##...";
  "###.#.###.";
  ".##..#..#.";
  "..##....#.";
  "#...##.###";
] |> List.iteri verify;;

module Locations = Set.Make(struct
  type t = int * int
  let compare = Pervasives.compare
end);;

let visited x y l = Locations.mem (x,y) l;;
let visit x y l = Locations.add (x,y) l;;

let shortest_path_to wall_at target =
  let rec go steps locations (x,y) =
    if x < 0 || y < 0 then None
    else if wall_at x y then None
    else if locations |> visited x y then None
    else if (x,y) = target then Some steps
    else
      [x+1,y; x,y+1; x-1,y; x,y-1]
      |> List.map (go (steps+1) (locations |> visit x y))
      |> List.keep Option.is_some |> function
        | [] -> None
        | valid -> valid |> List.min
  in go 0 Locations.empty (1,1)
;;

shortest_path_to (wall_at 10) (7,4)
|> function
  | None -> assert false
  | Some steps -> assert (steps = 11)
;;

let part1 () =
  shortest_path_to (wall_at 1364) (31,39)
  |> function
    | None -> assert false
    | Some steps -> Printf.printf "part1: %d\n%!" steps
;;

let visit_most wall_at max_steps =
  let rec go steps locations (x,y) =
    if x < 0 || y < 0 then locations
    else if wall_at x y then locations
    else if locations |> visited x y then locations
    else if steps > max_steps then locations
    else
      [x+1,y; x,y+1; x-1,y; x,y-1]
      |> List.map (go (steps+1) (locations |> visit x y))
      |> List.fold_left Locations.union locations
  in go 0 Locations.empty (1,1)
;;

let part2 () =
  visit_most (wall_at 1364) 50
  |> Locations.cardinal
  |> Printf.printf "part2: %d\n%!"
;;

part1 ();;
part2 ();;
