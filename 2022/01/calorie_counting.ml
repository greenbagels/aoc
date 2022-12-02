let rec maxcal cur_max acc =
  match read_line () with
  | exception End_of_file -> max cur_max acc
  | str ->
      if str = "" then maxcal (max cur_max acc) 0
      else maxcal cur_max (acc + int_of_string str)

let () = Printf.printf "%d\n" (maxcal 0 0)
