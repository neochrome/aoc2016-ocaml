#use "./lib.ml";;

type isotope =
  | Polonium
  | Thulium
  | Promethium
  | Ruthenium
  | Cobalt
  | Elerium
  | Dilithium
;;

let iso = function
  | Polonium   -> 0b0000001
  | Thulium    -> 0b0000010
  | Promethium -> 0b0000100
  | Ruthenium  -> 0b0001000
  | Cobalt     -> 0b0010000
  | Elerium    -> 0b0100000
  | Dilithium  -> 0b1000000
;;
let all  = 0b1111111;;
let none = 0;;

let rtg i = iso i lsl 7;;
let rtgs = List.map rtg;;
let chip = iso;;
let chips = List.map chip;;
let as_parts = List.fold_left ( lor ) none;;

let safe floor =
  let rtgs = floor lsr 7 and chips = floor land all in
  rtgs = none || lnot rtgs land chips = none
;;
let all_safe = Array.for_all safe;;
let unsafe floor = not (safe floor);;
let any_unsafe = Array.exists unsafe;;

assert (as_parts [] |> safe);;
assert (as_parts [rtg Polonium] |> safe);;
assert (as_parts [chip Polonium] |> safe);;
assert (as_parts [chip Cobalt; rtg Cobalt] |> safe);;
assert (as_parts [chip Cobalt; rtg Polonium] |> unsafe);;

type state = { elevator: int; floors: int array };;

let parts_on floor =
  let rec available n =
    if floor lsr n > 0 then
      let p = floor land (1 lsl n) in
      if p <> none then p :: available (n+1)
      else available (n+1)
    else []
  in available 0
;;

let parts_from floor =
  let all_parts = parts_on floor in
  all_parts
  |> List.map (fun a -> all_parts |> List.map (fun b -> [a;b]))
  |> List.flatten
  |> List.keep (function [a;b] when a <> b -> true | _ -> false)
  |> List.map (List.sort compare)
  |> List.sort_uniq compare
  |> List.rev_append (all_parts |> List.map (fun p -> [p]))
  |> List.map as_parts
;;

let remove_from = ( lxor );;
let add_to = ( lor );;
let move parts from_floor to_floor = Array.mapi (fun i floor ->
  if i = from_floor then remove_from floor parts
  else if i = to_floor then add_to floor parts
  else floor
);;

module BFSFloors = BFS.Make(struct
  type t = state;;
  type 'a hash = string;;
  let hash state =
    Printf.sprintf "%d|%d|%d|%d|%d"
    state.elevator
    state.floors.(0)
    state.floors.(1)
    state.floors.(2)
    state.floors.(3)
  ;;

  let possible_from state =
    let parts = parts_from state.floors.(state.elevator) in
    [1;-1]
    |> List.map (( + ) state.elevator)
    |> List.keep (fun e -> e >= 0 && e <= 3)
    |> List.map (fun e ->
      parts |> List.map (fun p -> {
        elevator=e;
        floors=move p state.elevator e state.floors
      }) |> List.keep (fun state -> state.floors |> all_safe)
    ) |> List.flatten
  ;;

  let is_final state =
    let top_floor = Array.length state.floors - 1 in
    state.floors
    |> Array.mapi (fun i parts -> if parts = none then i <> top_floor else i = top_floor)
    |> Array.fold_left ( && ) true
  ;;
end);;

let test_input = {
  elevator = 0;
  floors = [|
    chips [Polonium;Thulium] |> as_parts;
    rtgs [Polonium] |> as_parts;
    rtgs [Thulium] |> as_parts;
    none;
  |]
};;
assert (test_input.floors |> all_safe);;
test_input |> BFSFloors.search
|> function
  | None -> failwith "no solution found"
  | Some (steps,_) -> Printf.printf "test: %d\n%!" steps
;;

let part1_input = {
  elevator = 0;
  floors = [|
    (rtgs [Polonium;Thulium;Promethium;Ruthenium;Cobalt]) @ (chips [Thulium;Ruthenium;Cobalt]) |> as_parts;
    chips [Polonium;Promethium] |> as_parts;
    none;
    none;
  |]
};;
assert (part1_input.floors |> all_safe);;
part1_input |> BFSFloors.search
|> function
  | None -> failwith "no solution found"
  | Some (steps,_) -> Printf.printf "part1: %d\n%!" steps
;;

let part2_input = {
  elevator = 0;
  floors = [|
    (rtgs [Polonium;Thulium;Promethium;Ruthenium;Cobalt;Elerium;Dilithium]) @ (chips [Thulium;Ruthenium;Cobalt;Elerium;Dilithium]) |> as_parts;
    chips [Polonium;Promethium] |> as_parts;
    none;
    none;
  |]
};;
assert (part2_input.floors |> all_safe);;
part2_input |> BFSFloors.search
|> function
  | None -> failwith "no solution found"
  | Some (steps,_) -> Printf.printf "part2: %d\n%!" steps
;;
