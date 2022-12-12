
type monkey =
  {
    mutable count : int;
    items : int Queue.t;
    (* The operation performed on any input; None is used when the input only uses the old value *)
    (*operation : (int option -> int);*)
    (* Takes an input item and returns the index of the monkey receiving the result *)
    test : (int -> int * int)
  }

let parse_items () =
  let str = read_line () in
  let regex = Str.regexp {|Starting items: \(.+\)|} in
  let () = ignore Str.(search_forward regex str 0) in
  List.map int_of_string Str.(split (regexp ", ") (matched_group 1 str))
  |> List.to_seq |> Queue.of_seq

let parse_operation () =
  let str = read_line () in
  let regex = Str.regexp {|Operation: new = old \([\*\+]\) \(.+\)|} in
  let () = ignore Str.(search_forward regex str 0) in
  let (op, phrase) =
    match Str.matched_group 1 str with
    | "*" -> ( * ), "is multiplied by"
    | "+" -> ( + ), "increases by"
    | _ -> failwith "op parsing failed!"
  in match Str.matched_group 2 str with
    | "old" -> fun x -> (op x x) / 3
    | str -> fun x -> (op x (int_of_string str)) / 3

let parse_test () =
  (* parse the divisibility test line *)
  let strs = Array.init 3 (fun x -> read_line ()) in
  let regexps = Str.([|regexp {|Test: divisible by \([0-9]+\)|};
                      regexp {|If true: throw to monkey \([0-9]+\)|};
                      regexp {|If false: throw to monkey \([0-9]+\)|}|]) in
  let match_results = Array.init 3
      (fun n -> Str.(let () = ignore (search_forward regexps.(n) strs.(n) 0)
        in matched_group 1 strs.(n) |> int_of_string))
  in fun n ->
    if n mod match_results.(0) = 0 then
        (n, match_results.(1))
    else
      (n, match_results.(2))

let parse_monkeys () =
  let rec aux monkeys n =
    match read_line () with
    | exception End_of_file -> monkeys |> List.rev |> Array.of_list
    | "" -> aux monkeys n
    | _ ->
        (* Let's use this opportunity to finally use the Str regex functionality
         * to see how it compares to Re. But note that every regex in this
         * "inner loop" will be compiled every time, unless some optimizations
         * happen automagically *)
        let items = parse_items () in
        let op = parse_operation () in
        let test = parse_test () in
        let monkey = { count = 0; items = items; test = (fun x -> test (op x)) } in
        aux (monkey :: monkeys) (n+1)
  in aux [] 0

let print_monkeys monkeys =
  Array.iteri (fun i l ->
    Printf.printf "%d " i;
    Queue.iter (fun x ->
      Printf.printf "%d " x) l.items;
    print_endline "") monkeys;
    print_endline ""

let monkey_around monkeys =
  let rec aux cur_monkey =
    if Queue.is_empty cur_monkey.items then
      ()
    else
      let (worry, dest) = cur_monkey.test (Queue.pop cur_monkey.items) in
      cur_monkey.count <- (cur_monkey.count + 1);
      Queue.push worry monkeys.(dest).items;
      aux cur_monkey
  in
  for i = 1 to 20 do
    Array.iter aux monkeys;
     (*print_monkeys monkeys*)
  done

let () =
  let monkeys = parse_monkeys () in
    (*print_monkeys monkeys;*)
    let () = monkey_around monkeys in
      let vals = Array.map (fun x -> x.count) monkeys in
      Array.sort (fun x y -> compare y x) vals;
      Printf.printf "Amount of monkey business: %d\n" (vals.(0) * vals.(1))

