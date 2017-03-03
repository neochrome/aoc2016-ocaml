#use "./lib.ml";;

type node = { size:int; used:int; avail:int };;

let parse line =
  try
    Scanf.sscanf line "%_s %uT %uT %uT" (fun size used avail -> Some { size; used; avail })
  with _ -> None
;;

let non_empty n = n.used > 0;;
let fits a b = a.used <= b.avail;;
let viable_pairs nodes =
  let rec iter pairs heads = function
    | [] -> pairs
    | hd :: tl when non_empty hd -> iter (search pairs hd (heads @ tl)) (hd :: heads) tl
    | hd :: tl -> iter pairs (hd :: heads) tl
  and search pairs a = function
    | [] -> pairs
    | b :: tl when fits a b -> search ((a,b) :: pairs) a tl
    | _ :: tl -> search pairs a tl
  in iter [] [] nodes
;;

type grid = node array array;;
let as_grid w h nodes =
  Array.init h (fun y ->
    Array.init w (fun x -> List.nth nodes (x * h + y))
  )
;;

let wall_at grid my mx max_used min_size (x,y) =
  if x < 0 || y < 0 || x > mx || y > my then true
  else
    let n = grid.(y).(x) in
    n.used > max_used || n.size < min_size
;;

let walls_in grid =
  let r = grid.(0) in
  let g = r.(Array.length r - 1) in
  wall_at grid (Array.length grid - 1) (Array.length grid.(0) - 1) g.used g.used
;;

let pp grid =
  let g = (Array.length grid.(0) - 1, 0) in
  let wall_at = walls_in grid in
  grid |> Array.iteri (fun y row ->
    row |> Array.iteri (fun x n ->
      if (x,y) = g then
        Printf.printf "G"
      else if n.used = 0 then
        Printf.printf "E"
      else if wall_at (x,y) then
        Printf.printf "#"
      else
        Printf.printf ".";
    );
    Printf.printf "\n%!";
  );
  grid
;;

let find_empty_in grid =
  let rec scan = function
    | (x,y) when y = Array.length grid -> failwith "no empty found"
    | (x,y) when x = Array.length grid.(0) -> scan (0,y+1)
    | (x,y) when grid.(y).(x).used = 0 -> (x,y)
    | (x,y) -> scan (x+1,y)
  in scan (0,0)
;;

let solve grid =
  let start = find_empty_in grid in
  let target = (Array.length grid.(0) - 2,0) in
  let module BFSGrid = BFS.Make(struct
    type t = int * int;;
    type 'a hash = int * int;;
    let hash p = p;;
    let is_final p = p = target;;
    let possible_from (x,y) =
      [x,y-1;x,y+1;x-1,y;x+1,y]
      |> List.reject (walls_in grid)
    ;;
  end) in
  BFSGrid.search start
  |> function
    | None -> failwith "no path found"
    | Some (steps,(x,_)) ->
        steps   (* steps to move empty space*)
        + x * 5 (* 5 moves to bring G to the left*)
        + 1     (* final move *)
;;

File.open_in "./day22.input" (fun ch ->
  let nodes = Stream.of_lines ch
    |> Stream.map parse
    |> Stream.filter Option.is_some
    |> Stream.map Option.value_of
    |> Stream.to_list
  in
  nodes
  |> viable_pairs
  |> List.length
  |> Printf.printf "part1: %d\n%!";
  nodes
  |> as_grid 34 30
  (*|> pp*)
  |> solve
  |> Printf.printf "part2: %d\n%!";
);;
