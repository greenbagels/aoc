let score_item_prio ch =
  let code = Char.code ch
  and lower_start = Char.code 'a'
  and upper_start = Char.code 'A' in
  if code >= lower_start then 1 + code - lower_start
  else 27 + code - upper_start

let filter_group seqs =
  let head, tail = (List.hd seqs, List.tl seqs) in
  let tbl = Hashtbl.create 52 in
  let () =
    Seq.iter (fun ch -> Hashtbl.replace tbl ch false) head;
    List.iter
      (fun seq ->
        Seq.iter
          (fun ch ->
            match Hashtbl.find_opt tbl ch with
            | None -> ()
            | Some _ -> Hashtbl.replace tbl ch true)
          seq;
        Hashtbl.iter
          (fun k v ->
            if v then Hashtbl.replace tbl k false else Hashtbl.remove tbl k)
          tbl)
      tail
  in
  Hashtbl.fold (fun k v init -> init + score_item_prio k) tbl 0

let sum_groups k =
  let rec aux acc =
    match List.init k (fun x -> read_line () |> String.to_seq) with
    | exception End_of_file -> acc
    | seqs -> aux (acc + filter_group seqs)
  in
  aux 0

let () = Printf.printf "%d\n" (sum_groups (int_of_string Sys.argv.(1)))
