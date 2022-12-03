
let score_item_prio ch =
  let code = Char.code ch and
      lower_start = Char.code 'a' and
      upper_start = Char.code 'A' in
  if code >= lower_start then 1 + code - lower_start else 27 + code - upper_start

let parse_rucksack str =
  let len = String.length str in
  let exists ch =
    let rec aux i =
      match i - len/2 with
      | 0 -> false
      | _ -> if ch = str.[len/2 + i] then true else aux (i+1)
    in aux 0
  in

  let rec aux i =
    if exists str.[i] then
      score_item_prio str.[i]
    else
      aux (i+1)
  in aux 0

let sum_priorities () =
  let rec aux acc =
    match read_line () with
    | exception End_of_file -> acc
    | str -> aux (acc + parse_rucksack str)
  in aux 0

let () = Printf.printf "%d\n" (sum_priorities ())
