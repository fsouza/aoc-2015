open StdLabels
open MoreLabels
module StringMap = Map.Make (String)

let rec has_overlap = function
  | [] | [ _ ] -> false
  | (_, end1) :: ((start2, _) :: _ as tl) ->
      if end1 >= start2 then true else has_overlap tl

let repeats_with_no_overlap = function
  | [] | [ _ ] -> false
  | l ->
      l
      |> List.sort ~cmp:(fun (start1, _) (start2, _) ->
             Int.compare start1 start2)
      |> has_overlap
      |> not

let pairs str =
  let open Seq in
  let length = String.length str in
  let rec pairs_seq idx () =
    if idx = length then Nil
    else
      Cons
        ( (String.sub ~pos:(idx - 1) ~len:2 str, (idx - 1, idx)),
          pairs_seq (idx + 1) )
  in
  let add_item m (pair, pos) =
    match StringMap.find_opt pair m with
    | None -> StringMap.add ~key:pair ~data:[ pos ] m
    | Some items -> StringMap.add ~key:pair ~data:(pos :: items) m
  in
  pairs_seq 1
  |> Seq.fold_left add_item StringMap.empty
  |> StringMap.filter ~f:(fun _ items -> repeats_with_no_overlap items)
  |> StringMap.cardinal

let triples str =
  let open Seq in
  let length = String.length str in
  let rec triples_seq idx () =
    if idx = length then Nil
    else
      Cons
        ( str |> String.sub ~pos:(idx - 2) ~len:3 |> String.to_seq |> List.of_seq,
          triples_seq (idx + 1) )
  in
  let rec look_for_repeating_triples s =
    match s () with
    | Nil -> false
    | Cons ([ first; _; last ], _) when Char.equal first last -> true
    | Cons (_, s) -> look_for_repeating_triples s
  in
  triples_seq 2 |> look_for_repeating_triples

let is_nice str =
  let number_of_pairs = pairs str in
  let has_triples = triples str in
  number_of_pairs > 0 && has_triples

let () =
  Aoc.stdin
  |> Seq.filter is_nice
  |> Seq.fold_left (fun acc _ -> acc + 1) 0
  |> Printf.printf "%d\n"
