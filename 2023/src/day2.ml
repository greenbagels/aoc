let re =
  Re.(
    seq
      [
        group (rep1 digit);
        char ' ';
        group (alt [ str "red"; str "blue"; str "green" ]);
      ]
    |> compile)

let day1 fname =
  let file = open_in fname in
  let lines = Batteries.input_list file in
  List.mapi
    (fun n s ->
      let valid_game =
        String.split_on_char ';' s
        |> List.map (fun s ->
               let trial_draws =
                 Re.all re s
                 |> List.map (fun x ->
                        (int_of_string (Re.Group.get x 1), Re.Group.get x 2))
               in
               List.map
                 (fun (n, color) ->
                   Bool.to_int
                     (match color with
                     | "red" -> n <= 12
                     | "green" -> n <= 13
                     | "blue" -> n <= 14
                     | _ -> failwith "bad color"))
                 trial_draws
               |> List.fold_left ( * ) 1)
        |> List.fold_left ( * ) 1
      in
      (n + 1) * valid_game)
    lines
  |> List.fold_left ( + ) 0

let day2 fname =
  let file = open_in fname in
  let lines = Batteries.input_list file in
  let rec aux tbl = function
    (* We're just going to assume each line has all colors; if not, the unhandled
       exception will let us know lol *)
    | [] ->
        List.map (Hashtbl.find tbl) [ "red"; "blue"; "green" ]
        |> List.fold_left ( * ) 1
    | (n, color) :: l ->
        let k =
          match Hashtbl.find_opt tbl color with None -> 0 | Some k -> k
        in
        Hashtbl.replace tbl color (max n k);
        aux tbl l
  in
  List.map
    (fun s ->
      let tbl = Hashtbl.create 3 in
      String.split_on_char ';' s
      |> List.map (fun s ->
             Re.all re s
             |> List.map (fun x ->
                    (int_of_string (Re.Group.get x 1), Re.Group.get x 2)))
      |> List.flatten |> aux tbl)
    lines
  |> List.fold_left ( + ) 0

let () =
  List.iter (fun fn -> Printf.printf "%d\n" (fn Sys.argv.(1))) [ day1; day2 ]
