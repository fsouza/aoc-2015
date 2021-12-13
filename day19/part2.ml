open StdLabels
open MoreLabels
module StringSet = Set.Make (String)

let reverse_len_cmp s1 s2 = Int.compare (String.length s2) (String.length s1)

let parse_to_state (medicine, replacements) line =
  match String.split_on_char ~sep:'=' line with
  | [ lhs; rhs ] ->
      let lhs = String.trim lhs in
      let rhs = String.sub ~pos:2 ~len:(String.length rhs - 2) rhs in
      (medicine, (lhs, rhs) :: replacements)
  | [ medicine ] -> (medicine, replacements)
  | _ -> (medicine, replacements)

let index_from_opt str idx substr =
  let length = String.length str in
  let substr_length = String.length substr in
  let rec index_from_opt' idx =
    if idx = length then None
    else if idx + substr_length > length then None
    else
      let sub = String.sub ~pos:idx ~len:substr_length str in
      if sub = substr then Some idx else index_from_opt' (idx + 1)
  in
  index_from_opt' idx

let replace str idx search replace =
  let search_length = String.length search in
  let remain_length = String.length str - idx - search_length in
  [
    String.sub ~pos:0 ~len:idx str;
    replace;
    String.sub ~pos:(idx + search_length) ~len:remain_length str;
  ]
  |> String.concat ~sep:""

let replacements (medicine, replacements) =
  let rec count_replacements' acc idx search replacement =
    match index_from_opt medicine idx search with
    | Some idx ->
        let str = replace medicine idx search replacement in
        count_replacements' (StringSet.add str acc) (idx + 1) search replacement
    | None -> acc
  in
  List.fold_left ~init:StringSet.empty
    ~f:(fun acc (search, replacement) ->
      count_replacements' acc 0 search replacement)
    replacements

let find_shortest_path (medicine, map) =
  let rec find_path' visited orig =
    if orig = medicine then Some 0
    else if String.length orig > String.length medicine then None
    else
      let possible_steps =
        replacements (orig, map)
        |> StringSet.to_seq
        |> Seq.filter (fun s -> not @@ StringSet.mem s visited)
      in
      let visited = StringSet.add orig visited in
      possible_steps
      |> Seq.fold_left
           (fun acc s ->
             match (acc, find_path' visited s) with
             | None, None -> None
             | Some v, None -> Some v
             | None, Some v -> Some (v + 1)
             | Some acc, Some shortest -> Some (min acc (shortest + 1)))
           None
  in
  find_path' (StringSet.singleton "e") "e"

let () =
  Aoc.stdin
  |> Seq.fold_left parse_to_state ("", [])
  |> (fun (medicine, replacements) ->
       ( medicine,
         List.sort
           ~cmp:(fun (_, s1) (_, s2) -> reverse_len_cmp s1 s2)
           replacements ))
  |> find_shortest_path
  |> Option.value ~default:(-1)
  |> Printf.printf "%d\n"
