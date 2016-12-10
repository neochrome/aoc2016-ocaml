#use "./lib.ml";;

type position = { x:int; y:int };;

let keypad1 = [|
  [|" ";" ";" ";" ";" "|];
  [|" ";"1";"2";"3";" "|];
  [|" ";"4";"5";"6";" "|];
  [|" ";"7";"8";"9";" "|];
  [|" ";" ";" ";" ";" "|];
|];;

let keypad2 = [|
  [|" ";" ";" ";" ";" ";" ";" "|];
  [|" ";" ";" ";"1";" ";" ";" "|];
  [|" ";" ";"2";"3";"4";" ";" "|];
  [|" ";"5";"6";"7";"8";"9";" "|];
  [|" ";" ";"A";"B";"C";" ";" "|];
  [|" ";" ";" ";"D";" ";" ";" "|];
  [|" ";" ";" ";" ";" ";" ";" "|];
|];;

let move pos = function
  | 'U' -> { pos with y=pos.y - 1 }
  | 'D' -> { pos with y=pos.y + 1 }
  | 'L' -> { pos with x=pos.x - 1 }
  | 'R' -> { pos with x=pos.x + 1 }
  | _ -> assert false
;;

let discern_code keypad start instructions =
  let rec follow pos = function
    | [] -> pos
    | dir :: directions ->
      let constrained pos' = if keypad.(pos'.y).(pos'.x) = " " then pos else pos' in
      follow (move pos dir |> constrained) directions
  in
  instructions
  |> List.fold_left (fun (code, pos) instructions ->
    let pos' = String.to_list instructions |> follow pos in
    (keypad.(pos'.y).(pos'.x) :: code, pos')
  ) ([], start)
  |> fst |> List.rev |> List.fold_left ( ^ ) ""
;;

assert (
[
  "ULL";
  "RRDDD";
  "LURDL";
  "UUUUD";
] |> discern_code keypad1 {x=2; y=2}
= "1985");

assert (
[
  "ULL";
  "RRDDD";
  "LURDL";
  "UUUUD";
] |> discern_code keypad2 {x=1; y=3}
= "5DB3");

File.open_in "./day02.input" (fun ch ->
  let instructions = Stream.of_lines ch |> Stream.to_list in
  instructions
  |> discern_code keypad1 {x=2; y=2}
  |> Printf.printf "part1: the code is %s\n";
  instructions
  |> discern_code keypad2 {x=1; y=3}
  |> Printf.printf "part2: the code is %s\n";
);;
