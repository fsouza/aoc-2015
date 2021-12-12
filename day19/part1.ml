open StdLabels
open MoreLabels
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let parse_to_state (medicine, replacements) line =
  match String.split_on_char ~sep:'=' line with
  | [ lhs; rhs ] ->
      let lhs = String.trim lhs in
      let rhs = String.sub ~pos:2 ~len:(String.length rhs - 2) rhs in
      let curr =
        StringMap.find_opt lhs replacements |> Option.value ~default:[]
      in
      (medicine, StringMap.add ~key:lhs ~data:(rhs :: curr) replacements)
  | [ medicine ] -> (medicine, replacements)
  | _ -> (medicine, replacements)

let index_from_opt str idx substr =
  let length = String.length str in
  let substr_length = String.length substr in
  let rec index_from_opt' idx =
    if idx = length then None
    else if idx + substr_length >= length then None
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

let count_replacements (medicine, replacements) =
  let rec count_replacements' acc idx search = function
    | [] -> acc
    | hd :: tl as replacements -> (
        match index_from_opt medicine idx search with
        | Some idx ->
            let str = replace medicine idx search hd in
            count_replacements' (StringSet.add str acc) (idx + 1) search
              replacements
        | None -> count_replacements' acc 0 search tl)
  in
  StringMap.fold ~init:StringSet.empty
    ~f:(fun ~key ~data acc -> count_replacements' acc 0 key data)
    replacements
  |> StringSet.cardinal

let () =
  Aoc.stdin
  |> Seq.fold_left parse_to_state ("", StringMap.empty)
  |> count_replacements
  |> Printf.printf "%d\n"
