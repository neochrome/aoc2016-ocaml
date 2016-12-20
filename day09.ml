#use "./lib.ml";;

let decompressed_length ~recurse s =
  let rec count i s =
    let i' = i + 1 in
    if i = String.length s then 0
    else if s.[i] <> '(' then 1 + count i' s
    else
      let eom = String.index_from s i' ')' in
      let marker = String.sub s i' (eom - i') in
      let size,repeats = Scanf.sscanf marker "%dx%d" (fun s r -> (s,r)) in
      (if recurse then
        let seq = String.sub s (eom + 1) size in
        (repeats) * (count 0 seq)
      else size * repeats) + (count (eom + size + 1) s)
  in count 0 s
;;

let test1 = decompressed_length ~recurse:false;;
let test2 = decompressed_length ~recurse:true;;

let should_have_length expected actual =
  if actual <> expected then
    failwith (Printf.sprintf "got %d, expected %d" actual expected)
;;

"ADVENT" |> test1 |> should_have_length 6;;
"A(1x5)BC" |> test1 |> should_have_length 7;;
"(3x3)XYZ"  |> test1 |> should_have_length 9;;
"A(2x2)BCD(2x2)EFG" |> test1 |> should_have_length 11;;
"(6x1)(1x3)A" |> test1 |> should_have_length 6;;
"X(8x2)(3x3)ABCY" |> test1 |> should_have_length 18;;

"(3x3)XYZ"  |> test2 |> should_have_length 9;;
"X(8x2)(3x3)ABCY" |> test2 |> should_have_length 20;;
"(27x12)(20x12)(13x14)(7x10)(1x12)A" |> test2 |> should_have_length 241920;;
"(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN" |> test2 |> should_have_length 445;;

File.open_in "./day09.input" (fun ch ->
  let input = Stream.of_lines ch |> Stream.next in
  input |> decompressed_length ~recurse:false |> Printf.printf "part1: %d\n";
  input |> decompressed_length ~recurse:true |> Printf.printf "part2: %d\n";
);;
