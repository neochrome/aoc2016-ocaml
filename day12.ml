#use "./lib.ml";;

type register = A | B | C | D;;
type operand = Reg of register | Imm of int;;
type instruction =
  | Cpy of operand * register
  | Inc of register
  | Dec of register
  | Jnz of operand * int
;;

let try_with f = function
  | None -> begin try Some(f ()) with _ -> None end
  | Some _ as result -> result
;;

let reg_of = function 'a' -> A | 'b' -> B | 'c' -> C | 'd' -> D | _ -> assert false;;
let parse line =
  let scan fmt conv = fun () -> Scanf.sscanf line fmt conv in
  None
  |> try_with (scan "cpy %d %c" (fun v d -> Cpy (Imm v, reg_of d)))
  |> try_with (scan "cpy %c %c" (fun s d -> Cpy (Reg (reg_of s), reg_of d)))
  |> try_with (scan "inc %c" (fun r -> Inc (reg_of r)))
  |> try_with (scan "dec %c" (fun r -> Dec (reg_of r)))
  |> try_with (scan "jnz %d %d" (fun v o -> Jnz (Imm v, o)))
  |> try_with (scan "jnz %c %d" (fun r o -> Jnz (Reg (reg_of r), o)))
  |> function
    | None -> failwith (Printf.sprintf "bad line: %s" line)
    | Some ins -> ins
;;

let execute ?(regs=(0,0,0,0)) program =
  let rec execute pc regs =
    let a,b,c,d = regs in
    let jump o = execute (pc + o) in
    let next = jump 1 in
    let inc v = v + 1 in
    let dec v = v - 1 in
    let get = function A -> a | B -> b | C -> c | D -> d in
    let set reg v = match reg with
      | A -> (v,b,c,d)
      | B -> (a,v,c,d)
      | C -> (a,b,v,d)
      | D -> (a,b,c,v)
    in
    let value_of = function Reg r -> get r | Imm v -> v in
    if pc < 0 || pc >= Array.length program then regs
    else match program.(pc) with
      | Cpy (opr, reg) -> value_of opr |> set reg |> next
      | Inc reg -> get reg |> inc |> set reg |> next
      | Dec reg -> get reg |> dec |> set reg |> next
      | Jnz (opr, off) ->
        if value_of opr = 0 then next regs
        else jump off regs
  in execute 0 regs
;;

[|
  Cpy (Imm 41, A);
  Inc A;
  Inc A;
  Dec A;
  Jnz (Reg A, 2);
  Dec A;
|]
|> execute
|> (fun (a,b,c,d) -> assert (a = 42));;

File.open_in "./day12.input" (fun ch ->
  let program = Stream.of_lines ch |> Stream.map parse |> Stream.to_array in
  execute program |> (fun (a,_,_,_) -> Printf.printf "part1: %d\n%!" a);
  execute ~regs:(0,0,1,0) program |> (fun (a,_,_,_) -> Printf.printf "part2: %d\n%!" a);
);;
