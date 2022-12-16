
let tbl = Hashtbl.create 5000000

let map_pair f (x, y) = (f x, f y)

let l1metric (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

let exclusion_range y (sx, sy) (bx, by) =
  let dy = abs (sy - y) in
  let dsb = l1metric (sx, sy) (bx, by) in
  if dy > dsb then
    None
  else
    Some (sx - (dsb - dy), sx + (dsb - dy))

let parse row =
  let int_expr = {|\(-*[0-9]+\)|} in
  let regex_str =("Sensor at x=" ^ int_expr ^ ", y=" ^ int_expr ^
                          ": closest beacon is at x=" ^ int_expr ^ ", y=" ^ int_expr) in
  let regex = Str.regexp regex_str in
  let rec aux () =
    match read_line () with
    | exception End_of_file -> Hashtbl.length tbl
    | str ->
        (let open Str in
        let () = ignore (search_forward regex str 0) in
        let sensor = (matched_group 1 str, matched_group 2 str) |> map_pair int_of_string in
        let beacon = (matched_group 3 str, matched_group 4 str) |> map_pair int_of_string in
        match exclusion_range row sensor beacon with
        | None -> aux ()
        | Some (x1, x2) ->
          for i = x1 to x2 do
            Hashtbl.replace tbl (i, row) true
          done;
          Hashtbl.remove tbl beacon;
          aux ())
  in aux ()

let () =
  Printf.printf "%d\n" (parse (Sys.argv.(1) |> int_of_string))


