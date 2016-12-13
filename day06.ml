#use "./lib.ml";;

let examples = [
  "eedadn";
  "drvtee";
  "eandsr";
  "raavrd";
  "atevrs";
  "tsrnev";
  "sdttsa";
  "rasrtv";
  "nssdts";
  "ntnada";
  "svetve";
  "tesnvt";
  "vntsnd";
  "vrdear";
  "dvrsen";
  "enarar";
];;

let transpose columns text =
  let rec next_char i columns =
    match i, columns with
    | 0, [c] -> [text.[i] :: c]
    | _, [] -> next_char i [[]]
    | i, c :: columns -> (text.[i] :: c) :: next_char (i - 1) columns
  in next_char (String.length text - 1) columns
;;

let count choose column =
  let inc (c,n) = (c,n+1) in
  let rec count keep last chars =
    match chars, keep, last with
    | ch :: chars, _, None ->
      count keep (Some (ch,1)) chars
    | ch :: chars, _, Some last when ch = fst last ->
      count keep (Some (inc last)) chars
    | ch :: chars, None, Some last when ch <> fst last ->
      count (Some last) (Some (ch,1)) chars
    | ch :: chars, Some keep, Some last when ch <> fst last ->
      count (Some (choose keep last)) (Some (ch,1)) chars
    | [], None, Some last -> last |> fst
    | [], Some keep, Some last -> (choose keep last) |> fst
    | _, _, _ -> assert false
  in
  column |> List.fast_sort compare |> count None None
;;
let max a b = if snd b > snd a then b else a;;
let min a b = if snd a < snd b then a else b;;

let error_correct selection strings =
  let corrected = strings
    |> List.fold_left transpose []
    |> List.rev
    |> List.map selection
  in String.init (List.length corrected) (List.nth corrected)
;;
let error_correct_max = error_correct (count max);;
let error_correct_min = error_correct (count min);;

assert (error_correct_max examples = "easter");;
assert (error_correct_min examples = "advent");;

File.open_in "./day06.input" (fun ch ->
  let strings = Stream.of_lines ch |> Stream.to_list in
  strings |> error_correct_max |> Printf.printf "part1: %s\n";
  strings |> error_correct_min |> Printf.printf "part2: %s\n";
);;
