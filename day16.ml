let toggle = function
  | '0' -> '1'
  | '1' -> '0'
  | _ -> assert false
;;

let dragonize str =
  let len = String.length str in
  let rev = String.init len (fun i -> toggle str.[len - i - 1]) in
  str ^ "0" ^ rev
;;

assert (dragonize "1" = "100");;
assert (dragonize "0" = "001");;
assert (dragonize "11111" = "11111000000");;
assert (dragonize "111100001010" = "1111000010100101011110000");;

let rec fill str length =
  if String.length str >= length then String.sub str 0 length
  else fill (dragonize str) length
;;

let rec checksum str =
  let length = String.length str / 2 in
  let sum = String.init length (fun i ->
    match str.[i * 2], str.[i * 2 + 1] with
    | '0','0' | '1','1' -> '1'
    | _ -> '0'
  ) in
  if length mod 2 <> 0 then sum
  else checksum sum
;;

assert (fill "10000" 20 |> checksum = "01100");;

let input = "00111101111101000";;

fill input 272
|> checksum
|> Printf.printf "part1: %s\n%!";;

fill input 35651584
|> checksum
|> Printf.printf "part2: %s\n%!";;
