let day1 fname =
  let file = open_in fname in
  Batteries.input_list file
  |> List.map (fun s ->
         (* Toss some non-exhaustive pattern matching in just to keep it exciting
            for the compiler *)
         let (Some (a, b)) =
           String.fold_left
             (fun acc ch ->
               match ch with
               | '0' .. '9' -> (
                   match acc with
                   | None -> Some (ch, ch)
                   | Some (x, y) -> Some (x, ch))
               | _ -> acc)
             None s
         in
         String.make 1 a ^ String.make 1 b |> int_of_string)
  |> List.fold_left ( + ) 0

let day2 fname =
  let conv = function
    | "one" -> "1"
    | "two" -> "2"
    | "three" -> "3"
    | "four" -> "4"
    | "five" -> "5"
    | "six" -> "6"
    | "seven" -> "7"
    | "eight" -> "8"
    | "nine" -> "9"
    | s -> s
  in
  let rec all acc pos re str =
    match Re.exec ~pos re str with
    | exception Not_found -> List.rev acc
    | g ->
        let s = Re.Group.get g 0 in
        let start = Re.Group.start g 0 in
        let stop = Re.Group.stop g 0 in
        all (s :: acc) (if start + 1 = stop then stop else stop - 1) re str
  in
  let re =
    Re.(
      digit
      :: List.map str
           [
             "one";
             "two";
             "three";
             "four";
             "five";
             "six";
             "seven";
             "eight";
             "nine";
           ]
      |> alt |> compile)
  in
  let file = open_in fname in
  Batteries.input_list file
  |> List.map (fun s ->
         let l = all [] 0 re s |> List.map conv in
         let n =
           int_of_string (List.nth l 0 ^ List.nth l (List.length l - 1))
         in
         Printf.printf "%d\n" n;
         n)
  |> List.fold_left ( + ) 0

let () =
  List.iter (fun fn -> Printf.printf "%d\n" (fn Sys.argv.(1))) [ day1; day2 ]
