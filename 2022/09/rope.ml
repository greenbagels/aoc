
let tbl = Hashtbl.create 500

let dir_to_dr = function
  | 'L' -> (-1,0)
  | 'R' -> (1,0)
  | 'D' -> (0,-1)
  | 'U' -> (0,1)
  | _ -> failwith "invalid instruction"

(* i'm too lazy to use a real vector math library,
 * yet not lazy enough to avoid implementing my own *)
let axpy2 a (x1, x2) (y1, y2) =
  (a*x1 + y1, a*x2 + y2)

let scale a x =
  axpy2 a x (0, 0)

let (++) x y =
  axpy2 1 x y

let (--) x y =
  axpy2 (-1) y x

let ( *+ ) (x1, x2) (y1, y2) =
  x1 * y1 + x2 * y2

let sign2 (x1, x2) =
  let sign x = if x > 0 then 1 else if x < 0 then -1 else 0 in
  (sign x1, sign x2)

let part_one () =
    let rec step h r dr n =
      let () = Hashtbl.replace tbl (h -- r) 1 in
      if n = 0 then
        (h, r)
      else
        let new_h = h ++ dr in
        let new_r = r ++ dr in
        match new_r *+ new_r with
        | 4 -> step new_h r dr (n-1)
        | 5 -> step new_h (new_r -- (sign2 new_r)) dr (n-1)
        | _ -> step new_h new_r dr (n-1)
    in

  let rec aux (h, r) =
    match read_line () with
    | exception End_of_file -> ()
    | str ->
      let dr = dir_to_dr str.[0] and
          n = String.sub str 2 (String.length str - 2) |> int_of_string in
          step h r dr n |> aux
  in
  aux ((0,0), (0,0));
  Printf.printf "%d\n" (Hashtbl.length tbl)

let () = part_one ()
