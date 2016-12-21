#use "./lib.ml";;

type destination = Output of int | Bot of int;;
type bot = { low: destination; high: destination; payload:int option; };;

module IntMap = Map.Make(struct
  type t = int
  let compare = Pervasives.compare
end);;

let register id low high bots = bots |> IntMap.add id { low; high; payload=None };;
let bot b = Bot b;;
let output o = Output o;;

let rec send value dest (bots, outputs) =
  match dest with
  | Output o -> bots, outputs |> IntMap.add o value
  | Bot id ->
    let bot = IntMap.find id bots in
    match bot.payload with
    | None -> bots |> IntMap.add id { bot with payload=Some value }, outputs
    | Some payload ->
      (if (value = 17 && payload = 61)
       || (value = 61 && payload = 17)
      then Printf.printf "part1: %d\n" id);
      (bots |> IntMap.add id { bot with payload=None }, outputs)
      |> send (min value payload) bot.low
      |> send (max value payload) bot.high
;;

let try_with f = function
  | None -> begin try Some(f ()) with _ -> None end
  | Some _ as result -> result
;;

let parse line (bots, sends) =
  let scan fmt conv = fun () -> Scanf.sscanf line fmt conv in
  match None |> try_with (scan "value %d goes to bot %d" (fun v b -> send v (Bot b))) with
  | Some send -> (bots, send :: sends)
  | None ->
    let register flo fhi = fun id lo hi -> register id (flo lo) (fhi hi) in
    None
    |> try_with (scan "bot %d gives low to bot %d and high to bot %d" (register bot bot))
    |> try_with (scan "bot %d gives low to bot %d and high to output %d" (register bot output))
    |> try_with (scan "bot %d gives low to output %d and high to bot %d" (register output bot))
    |> try_with (scan "bot %d gives low to output %d and high to output %d" (register output output))
    |> function
      | None -> assert false
      | Some registration -> (registration bots, sends)
;;

File.open_in "./day10.input" (fun ch ->
  let bots, sends = Stream.of_lines ch |> Stream.fold parse (IntMap.empty, []) in
  let bots', outputs = List.fold_right ( @@ ) sends (bots, IntMap.empty) in
  IntMap.fold (fun id v acc -> if id < 3 then v * acc else acc) outputs 1
  |> Printf.printf "part2: %d\n"
);;
