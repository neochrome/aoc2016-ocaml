#use "./lib.ml";;

let hash s = Digest.string s |> Digest.to_hex;;
type direction = Up | Down | Left | Right;;
let directions = [Up;Down;Left;Right];;
let as_path = function
  | Up -> "U"
  | Down -> "D"
  | Left -> "L"
  | Right -> "R"
;;
let move (x,y) path d =
  (match d with
    | Up -> (x,y-1)
    | Down -> (x,y+1)
    | Left -> (x-1,y)
    | Right -> (x+1,y)
  ), path ^ (as_path d)
;;

let is_open = function 'b'..'f' -> true | _ -> false;;
let open_doors path =
  let h = hash path in
  directions |> List.filteri (fun i _ -> is_open h.[i])
;;

assert ("hijkl" |> open_doors = [Up;Down;Left]);;

let except prefix str =
  let str_len = String.length str in
  let pre_len = String.length prefix in
  String.sub str pre_len (str_len - pre_len)
;;

let vault = (4,4);;
let start = (1,1);;
let within (x,y) = x >= 1 && x <= (fst vault) && y >= 1 && y <= (snd vault);;
let input = "bwnlcvfs";;

let shortest passcode =
  let q = Queue.create () in
  let rec search () =
    if q |> Queue.is_empty then failwith "no solution found"
    else
      let p,path = q |> Queue.pop in
      if p = vault then path
      else begin
        open_doors path
        |> List.map (move p path)
        |> List.filter (fun (p', _) -> within p')
        |> List.iter (fun x -> q |> Queue.push x);
        search ()
      end
  in
  q |> Queue.push (start, passcode);
  search ()
  |> except passcode
;;

assert (shortest "ihgpwlah" = "DDRRRD");;
assert (shortest "kglvqrro" = "DDUDRLRRUDRD");;

let longest_of a b = if String.length a > String.length b then a else b;;

assert (longest_of "abc" "defg" = "defg");;
assert (longest_of "hijkl" "mno" = "hijkl");;

let longest passcode =
  let q = Queue.create () in
  let rec search longest_so_far =
    if q |> Queue.is_empty then longest_so_far
    else
      let p,path = q |> Queue.pop in
      if p = vault then search (longest_of longest_so_far path)
      else begin
        open_doors path
        |> List.map (move p path)
        |> List.filter (fun (p', _) -> within p')
        |> List.iter (fun x -> q |> Queue.push x);
        search longest_so_far
      end
  in
  q |> Queue.push (start, passcode);
  search ""
  |> except passcode
  |> String.length
;;

assert (longest "ihgpwlah" = 370);;
assert (longest "kglvqrro" = 492);;
assert (longest "ulqzkmiv" = 830);;

shortest input |> Printf.printf "part1: %s\n%!";;
longest input |> Printf.printf "part2: %d\n%!";;
