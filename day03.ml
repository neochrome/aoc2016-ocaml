#use "./lib.ml";;

let parse line = Scanf.sscanf line "%_[ ]%d%_[ ]%d%_[ ]%d" (fun a b c -> (a, b, c));;

let possible_tri (a, b, c) =
  a + b > c && a + c > b && b + c > a
;;

let transpose stream =
  let rec next i =
    match Stream.peek stream with
    | None -> None
    | _ ->
      let a1,b1,c1 = stream |> Stream.next |> parse
      and a2,b2,c2 = stream |> Stream.next |> parse
      and a3,b3,c3 = stream |> Stream.next |> parse
      in Some ((a1,a2,a3) :: (b1,b2,b3) :: (c1,c2,c3) :: [])
  in Stream.from next
;;

File.open_in "./day03.input" (fun ch ->
  Stream.of_lines ch
  |> Stream.map parse
  |> Stream.filter possible_tri
  |> Stream.count
  |> Printf.printf "part1: %d\n"
);;

File.open_in "./day03.input" (fun ch ->
  Stream.of_lines ch
  |> transpose
  |> Stream.map (fun lines ->
    lines |> List.filter possible_tri |> List.length
  ) |> Stream.reduce ( + ) 0
  |> Printf.printf "part2: %d\n"
);;
