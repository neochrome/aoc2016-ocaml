#use "./lib.ml";;

let ensure check ips =
  ips |> List.iter (fun (ip, result) ->
    if not (check ip = result) then
    Printf.sprintf "%s -> %b" ip result |> failwith
  )
;;

let supports_tls ip =
  let rec has_abba = function
    | '[' :: tl, _, abba -> has_abba (tl, true, abba)
    | ']' :: tl, _, abba -> has_abba (tl, false, abba)
    | a :: b :: c :: d :: tl, true, _ when a = d && b = c && a <> b -> false
    | a :: b :: c :: d :: tl, false, _ when a = d && b = c && a <> b -> has_abba (tl, false, true)
    | _ :: tl, in_brackets, abba -> has_abba (tl, in_brackets, abba)
    | [], _, true -> true
    | _ -> false
  in has_abba (ip |> String.to_list, false, false)
;;
[
  "abba[mnop]qrst", true;
  "abba[mnop]qr[mnnm]st", false;
  "abcd[bddb]xyyx", false;
  "aaaa[qwer]tyui", false;
  "ioxxoj[asdfgh]zxcvbn", true;
] |> ensure supports_tls;;

let supports_ssl ip =
  let start = (ip |> String.to_list, false) in
  let rec has_aba = function
    | '[' :: tl, _ -> has_aba (tl, true)
    | ']' :: tl, _ -> has_aba (tl, false)
    | a :: b :: c :: tl, false when a = c && a <> b ->
      if has_bab (a,b) start then true
      else has_aba (b :: c :: tl, false)
    | _ :: tl, in_brackets -> has_aba (tl, in_brackets)
    | _ -> false
  and has_bab (x,y) = function
    | '[' :: tl, _ -> has_bab (x,y) (tl, true)
    | ']' :: tl, _ -> has_bab (x,y) (tl, false)
    | a :: b :: c :: tl, true when a = y && c = y && b = x -> true
    | _ :: tl, in_brackets -> has_bab (x,y) (tl, in_brackets)
    | _ -> false
  in has_aba start
;;
[
  "aba[bab]xyz", true;
  "xyx[xyx]xyx", false;
  "aaa[kek]eke", true;
  "zazbz[bzb]cdb", true;
] |> ensure supports_ssl;;

File.open_in "./day07.input" (fun ch ->
  let ips = Stream.of_lines ch |> Stream.to_list in
  ips |> List.filter supports_tls
  |> List.length |> Printf.printf "part1: %d\n";
  ips |> List.filter supports_ssl
  |> List.length |> Printf.printf "part2: %d\n";
);;
