open StdLabels
open MoreLabels
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let parse_units gain_or_lose amount =
  let amount = int_of_string amount in
  let mult = if gain_or_lose = "gain" then 1 else -1 in
  amount * mult

let normalize_name name =
  if String.ends_with ~suffix:"." name then
    String.sub ~pos:0 ~len:(String.length name - 1) name
  else name

let parse str =
  match String.split_on_char ~sep:' ' str with
  | [
   name1;
   "would";
   gain_or_lose;
   amount;
   "happiness";
   "units";
   "by";
   "sitting";
   "next";
   "to";
   name2;
  ] -> Some (name1, normalize_name name2, parse_units gain_or_lose amount)
  | _ -> None

let people map = map |> StringMap.to_seq |> Seq.map fst |> StringSet.of_seq

let add_to_map map (name1, name2, amount) =
  let curr = StringMap.find_opt name1 map |> Option.value ~default:[] in
  StringMap.add ~key:name1 ~data:((name2, amount) :: curr) map

let add_me map =
  let all_names =
    map
    |> StringMap.to_seq
    |> Seq.map (fun (name, _) -> (name, 0))
    |> List.of_seq
  in
  map
  |> StringMap.add ~key:"me" ~data:all_names
  |> StringMap.fold
       ~f:(fun ~key ~data map ->
         if key != "me" then StringMap.add ~key ~data:(("me", 0) :: data) map
         else StringMap.add ~key ~data map)
       ~init:map

let find_amount map orig dest =
  map
  |> StringMap.find orig
  |> List.find ~f:(fun (name2, _) -> name2 = dest)
  |> snd

let find_answer map =
  let find_answer' first_key =
    let rec calc acc key to_visit =
      let aux =
        if StringSet.cardinal to_visit = 1 then
          let last_one = StringSet.max_elt to_visit in
          find_amount map last_one first_key
          + find_amount map first_key last_one
        else 0
      in
      if StringSet.is_empty to_visit then acc
      else
        map
        |> StringMap.find key
        |> List.to_seq
        |> Seq.filter (fun (name2, _) -> StringSet.mem name2 to_visit)
        |> Seq.map (fun (name2, amount) ->
               let rev = find_amount map name2 key in
               calc
                 (acc + amount + rev + aux)
                 name2
                 (StringSet.remove name2 to_visit))
        |> Seq.fold_left max min_int
    in
    calc 0 first_key (map |> people |> StringSet.remove first_key)
  in
  StringMap.fold ~init:min_int
    ~f:(fun ~key ~data:_ acc ->
      let best_from_here = find_answer' key in
      max acc best_from_here)
    map

let print_map map =
  StringMap.iter
    ~f:(fun ~key ~data ->
      print_endline key;
      List.iter
        ~f:(fun (name2, amount) -> Printf.printf "  %s %d\n" name2 amount)
        data;
      print_newline ())
    map;
  map

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.fold_left add_to_map StringMap.empty
  |> add_me
  |> print_map
  |> find_answer
  |> Printf.printf "%d\n"
