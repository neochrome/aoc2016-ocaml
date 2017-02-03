type disc = { size: int; current: int };;

let rotate steps disc = { disc with current = (disc.current + steps) mod disc.size };;
let will_be_open steps disc = (rotate steps disc).current = 0;;
assert (will_be_open 1 { size=5; current=4 } = true);;
assert (will_be_open 2 { size=2; current=1 } = false);;

let will_all_be_open discs =
  discs
  |> List.mapi (fun i disc -> disc |> will_be_open (i + 1))
  |> List.fold_left ( && ) true
;;

let example = [
  { size=5; current=4 };
  { size=2; current=1 };
];;

assert (example |> will_all_be_open = false);;
assert (example |> List.map (rotate 5) |> will_all_be_open);;

let rec solve time discs =
  if discs |> will_all_be_open then time
  else discs |> List.map (rotate 1) |> solve (time + 1)
;;

assert (example |> solve 0 = 5);;

let input = [
  { size=13; current=10 };
  { size=17; current=15 };
  { size=19; current=17 };
  { size=7; current=1 };
  { size=5; current=0 };
  { size=3; current=1 };
];;

input |> solve 0 |> Printf.printf "part1: %d\n%!";;
input @ [{ size=11; current=0 }] |> solve 0 |> Printf.printf "part2: %d\n%!";;
