let hibit v =
  (([1;2;4;8;16;32]
    |> List.fold_left (fun v i -> v lor (v lsr i)) (v-1)
  )+1) lsr 1
;;

assert (hibit 170 = 128);;
assert (hibit 0b101001 = 0b100000);;

let part1 n = let l = n - (hibit n) in 2 * l + 1;;
assert (part1 5 = 3);;

type elf = { id: int; mutable prev: int; mutable next: int };;
let part2 n =
  let elves = Array.init n (fun id ->
    { id; prev=(id - 1 + n) mod n; next=(id + 1) mod n; }
  ) in
  let unlink i =
    let elf = elves.(i) in
    elves.(elf.prev).next <- elf.next;
    elves.(elf.next).prev <- elf.prev;
    ()
  in
  let rec next i = function
    | 0 -> i
    | s -> next (elves.(i).next) (s - 1)
  in
  let rec iter i h = function
    | 1 -> i
    | n ->
      unlink h;
      iter (next i 1) (next h (n mod 2 + 1)) (n - 1)
  in
  iter 0 (n / 2) n |> (+) 1
;;
assert (part2 5 = 2);;

let input = 3014603;;
part1 input |> Printf.printf "part1: %d\n%!";;
part2 input |> Printf.printf "part2: %d\n%!";;
