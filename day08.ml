#use "./lib.ml";;

let try_with f = function
  | None -> begin try Some (f ()) with _ -> None end
  | Some _ as result -> result
;;

let scan line fmt op = fun () ->
  Scanf.sscanf line fmt op
;;

type lcd = { w:int; h:int; leds:bool array array };;

let show lcd =
  let open Printf in
  print_endline (String.make lcd.w '-');
  Array.iter (fun row ->
    row
    |> Array.map (function true -> '#' | false -> '.')
    |> Array.iter (printf "%c");
    printf "\n"
  ) lcd.leds;
  print_endline (String.make lcd.w '-');
  lcd
;;

let init w h = { w; h; leds=Array.make_matrix h w false };;

let rect w h lcd =
  { lcd with leds=Array.mapi (fun i row ->
    if i < h then Array.mapi (fun col s -> col < w || s) row
    else row
  ) lcd.leds }
;;

let rec rotate_column x s lcd =
  let lookup r = lcd.leds.((r + lcd.h - s) mod lcd.h).(x) in
  { lcd with leds=Array.mapi (fun r row ->
    Array.mapi (fun c s ->
      if c <> x then s
      else lookup r
    ) row
  ) lcd.leds }
;;

let rotate_row y s lcd =
  let lookup c = lcd.leds.(y).((c + lcd.w - s) mod lcd.w) in
  { lcd with leds=Array.mapi (fun r row ->
    if r <> y then row
    else Array.mapi (fun c _ -> lookup c) row
  ) lcd.leds }
;;

let to_list lcd =
  lcd.leds |> Array.map Array.to_list
  |> Array.to_list
  |> List.flatten
;;

let parse line =
  let scan = scan line in
  None
  |> try_with (scan "rect %dx%d" rect)
  |> try_with (scan "rotate column x=%d by %d" rotate_column)
  |> try_with (scan "rotate row y=%d by %d" rotate_row)
  |> function
    | Some op -> op
    | None -> failwith (Printf.sprintf "Invalid line: %s" line)
;;

File.open_in "./day08.input" (fun ch ->
  Stream.of_lines ch
  |> Stream.map parse
  |> Stream.fold (fun op lcd -> op lcd) (init 50 6)
  |> show
  |> to_list
  |> List.map (function true -> 1 | false -> 0)
  |> List.fold_left ( + ) 0
  |> Printf.printf "part1: %d\n"
);;
