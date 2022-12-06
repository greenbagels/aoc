type part = One | Two

let re =
  Re.(
    compile
      (seq
         [
           group (rep1 digit);
           char '-';
           group (rep1 digit);
           char ',';
           group (rep1 digit);
           char '-';
           group (rep1 digit);
         ]))

let is_subset part coords =
  match coords with
  | [ a; b; c; d ] -> (
      match part with
      (* If we sorted the coordinate pairs (a, b) and (c, d) by reordering the
       * two pairs to guarantee a <= c, then we can eliminate the right half of
       * the logical OR. This saves us 2 comparisons and 2 boolean operations,
       * but for clarity we won't do this micro-optimization. *)
      | One -> (a <= c && b >= d) || (a >= c && b <= d)
      | Two -> (b >= c && a <= d) || (d >= a && c <= b))
  | _ -> failwith "no"

let parse_assignments part =
  let ic = open_in "input" in
  let rec aux acc =
    match input_line ic with
    | exception End_of_file -> acc
    | str ->
        let matches = Re.exec re str in
        let groups =
          List.init 4 (fun k -> Re.Group.get matches (k + 1) |> int_of_string)
        in
        aux (acc + Bool.to_int (is_subset part groups))
  in
  aux 0

let () =
  Printf.printf "%d\n" (parse_assignments One);
  Printf.printf "%d\n" (parse_assignments Two)
