#use "./lib.ml";;

let solve max_ip ranges =
  let rec iter mn allowed ip = function
    | [] -> Option.value_of mn,allowed + max_ip - ip + 1
    | (lo,hi) :: ranges ->
      let allowed' = allowed + (max 0 (lo - ip))
      and ip' = max ip (hi + 1)
      and mn' = if mn = None && ip < lo then Some ip else mn
      in iter mn' allowed' ip' ranges
  in ranges |> List.sort compare |> iter None 0 0
;;

assert (solve 9 [5,8;0,2;4,7] = (3,2));;
assert (solve 9 [5,9;1,3;4,7] = (0,1));;
assert (solve 9 [5,8;0,1;4,7] = (2,3));;
assert (solve 9 [5,7;0,1;4,8] = (2,3));;

let parse line = Scanf.sscanf line "%u-%u" (fun first last -> (first,last));;
File.open_in "./day20.input" (fun ch ->
  let ranges =
    Stream.of_lines ch
    |> Stream.map parse
    |> Stream.to_list
  in
  solve 4294967295 ranges
  |> fun (pt1,pt2) -> Printf.printf "part1: %d\npart2: %d\n" pt1 pt2;
);;
