#use "./lib.ml";;

type register = A | B | C | D;;
type operand = Reg of register | Imm of int;;
type instruction =
  | Cpy of operand * register
  | Inc of register
  | Dec of register
  | Jnz of operand * operand
  | Tgl of operand
  | Skip of instruction
  | Mul of operand * operand * register * register * register
;;

let toggle = function
  | Inc r -> Dec r
  | Dec r | Tgl (Reg r) -> Inc r
  | Tgl (Imm _) as ins -> Skip ins
  | Jnz (_,Imm _) as ins -> Skip ins
  | Jnz (a,Reg b) -> Cpy (a,b)
  | Cpy (a,b) -> Jnz (a,Reg b)
  | Skip ins -> ins
  | Mul _ as mul -> mul
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
  |> try_with (scan "jnz %d %d" (fun c o -> Jnz (Imm c, Imm o)))
  |> try_with (scan "jnz %c %d" (fun c o -> Jnz (Reg (reg_of c), Imm o)))
  |> try_with (scan "jnz %d %c" (fun c o -> Jnz (Imm c, Reg (reg_of o))))
  |> try_with (scan "jnz %c %c" (fun c o -> Jnz (Reg (reg_of c), Reg (reg_of o))))
  |> try_with (scan "tgl %c" (fun a -> Tgl (Reg (reg_of a))))
  |> try_with (scan "tgl %d" (fun a -> Tgl (Imm a)))
  |> function
    | None -> failwith (Printf.sprintf "bad line: %s" line)
    | Some ins -> ins
;;

let optimize instructions =
  let rec optimize program = function
    | [] -> program |> List.rev
    | Cpy (o1, r2) ::
      Cpy (Imm 0, res) ::
      Cpy (o2, r1) ::
      Inc res' ::
      Dec r1' ::
      Jnz (Reg r1'', Imm -2) ::
      Dec r2' ::
      Jnz (Reg r2'', Imm -5) ::
      instructions when
      res = res' &&
      r1 = r1' && r1 = r1'' &&
      r2 = r2' && r2 = r2'' -> optimize (
        Mul (o1, o2, res', r1, r2) ::
        Jnz (Imm 0, Imm 0) ::
        Jnz (Imm 0, Imm 0) ::
        Jnz (Imm 0, Imm 0) ::
        Jnz (Imm 0, Imm 0) ::
        Jnz (Imm 0, Imm 0) ::
        Jnz (Imm 0, Imm 0) ::
        Jnz (Imm 0, Imm 0) ::
        program) instructions
    | ins :: instructions -> optimize (ins :: program) instructions
  in optimize [] instructions
;;

let execute ?(regs=(0,0,0,0)) instructions =
  let program = instructions |> Array.of_list in
  let rec execute pc regs =
    let jump o = execute (pc + o) in
    let next = jump 1 in
    let inc v = v + 1 in
    let dec v = v - 1 in
    let get reg (a,b,c,d) = match reg with A -> a | B -> b | C -> c | D -> d in
    let set reg v (a,b,c,d) = match reg with
      | A -> (v,b,c,d)
      | B -> (a,v,c,d)
      | C -> (a,b,v,d)
      | D -> (a,b,c,v)
    in
    let clear reg regs = set reg 0 regs in
    let value_of op regs = match op with Reg r -> regs |> get r | Imm v -> v in
    if pc < 0 || pc >= Array.length program then regs
    else match program.(pc) with
      | Cpy (op, reg) -> regs |> set reg (regs |> value_of op) |> next
      | Inc reg -> regs |> set reg (regs |> get reg |> inc) |> next
      | Dec reg -> regs |> set reg (regs |> get reg |> dec) |> next
      | Jnz (cmp,off) ->
        if regs |> value_of cmp = 0 then next regs
        else jump (regs |> value_of off) regs
      | Tgl off ->
        let ref = pc + (regs |> value_of off) in
        if ref >= 0 && ref < Array.length program then
          program.(ref) <- toggle program.(ref);
        next regs
      | Skip _ -> next regs
      | Mul (o1,o2,res,r1,r2) ->
        regs
        |> set res ((regs |> value_of o1) * (regs |> value_of o2))
        |> clear r1 |> clear r2
        |> next
  in execute 0 regs
;;

[
  Cpy (Imm 2, A);
  Tgl (Reg A);
  Tgl (Reg A);
  Tgl (Reg A);
  Cpy (Imm 1, A);
  Dec A;
  Dec A;
]
|> execute
|> (fun (a,b,c,d) -> assert (a = 3));;

File.open_in "./day23.input" (fun ch ->
  let instructions = Stream.of_lines ch |> Stream.map parse |> Stream.to_list in
  instructions
  |> execute ~regs:(7,0,0,0)
  |> (fun (a,_,_,_) -> Printf.printf "part1: %d\n%!" a);
  instructions
  |> optimize
  |> execute ~regs:(12,0,0,0)
  |> (fun (a,_,_,_) -> Printf.printf "part2: %d\n%!" a);
);;

