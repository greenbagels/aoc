
type part =
  | Part_one
  | Part_two

let regex = Re.compile Re.(seq [str "move "; group (rep1 digit); str " from "; group digit; str " to "; group digit])

let parse_crates () =
  let rec aux bins =
    let rec parse_crate line k =
      (match String.sub line (4 * k) 3 with
      | "   " -> ()
      | s -> bins.(k) <- (s.[1] :: bins.(k)));
      match line.[4 * (k + 1)] with
      | exception Invalid_argument _ -> ()
      | _ -> parse_crate line (k+1)
    in match read_line () with
    | s ->
        (match String.index s '[' with
        (* index line found *)
        | exception Not_found -> ()
        | _ ->
            parse_crate s 0;
            aux bins)
  in let bins = Array.make 9 [] in
  let () = aux bins in
  bins

let rev_bins = Array.map List.rev

let print_crates bins =
  (* We are making an implicit assumption that no stack will be empty at the end! *)
  Array.iter (fun l -> (Printf.printf "%c") (List.hd l)) bins;
  print_endline ""

let rec parse_procedure bins part =
  let f = match part with
  | Part_one -> Fun.id
  | Part_two -> List.rev
  in
  let rec move_crates (src, dst, qty) tmp =
    match qty with
    | 0 -> bins.(dst) <- f tmp @ bins.(dst)
    | n ->
        let (src_head, src_tail) = (List.hd bins.(src), List.tl bins.(src)) in
        let new_tmp = (src_head :: tmp) in
        bins.(src) <- src_tail;
        move_crates (src, dst, qty - 1) new_tmp
  in
  match read_line () with
  | exception End_of_file -> ()
  | s ->
    let groups = Re.exec regex s in
    (* ocaml complains about partial matching here, despite the list always
     * returning either 3 elements or an exception. Can we preserve the
     * expressiveness without invoking compiler warnings? *)
    let [src; dst; qty] = List.map int_of_string Re.Group.([get groups 2; get groups 3; get groups 1]) in
    move_crates (src - 1, dst - 1, qty) [];
    parse_procedure bins part

let () =
  let bins = parse_crates () |> rev_bins in
  (* Skip the blank line between the initial layout and the procedure *)
  ignore (read_line ());
  parse_procedure bins Part_two;
  print_crates bins

