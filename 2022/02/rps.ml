
let score_outcome = function
  | 'A', 'Y' | 'B', 'Z' | 'C', 'X' -> 6
  | 'A', 'X' | 'B', 'Y' | 'C', 'Z' -> 3
  | _ -> 0

let score_symbol = function
  | 'X' -> 1
  | 'Y' -> 2
  | 'Z' -> 3
  | _ -> failwith "bad"

let score_tournament () =
  let rec aux acc =
    match read_line () with
    | exception End_of_file -> acc
    | str ->
        let (x, y) = (str.[0], str.[2]) in
        aux (acc + score_outcome (x,y) + score_symbol y)
  in aux 0

let () = Printf.printf "%d\n" (score_tournament ())
