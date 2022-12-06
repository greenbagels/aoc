
type part =
| One
| Two

let calc_solution part =
  let rec find_nonrepeat str idx =
    let k =
      match part with
      | One -> 4
      | Two -> 14
    in
    let tbl = Hashtbl.create k in
    let () =
    for i = 0 to (k-1) do
      match Hashtbl.find_opt tbl str.[idx + i] with
      | None -> Hashtbl.add tbl str.[idx + i] 1
      | Some n -> Hashtbl.replace tbl str.[idx + i] (n + 1)
    done in
    match Hashtbl.fold (fun k v init -> v * init) tbl 1 with
    | 1 ->
        idx + k
    | n ->
        find_nonrepeat str (idx + 1)
  in
  let ic = open_in "input" in
  let input_str = input_line ic in
  find_nonrepeat input_str 0

let () = Printf.printf "%d\n" (calc_solution One)
let () = Printf.printf "%d\n" (calc_solution Two)
