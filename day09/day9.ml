open StdLabels
open MoreLabels
module StringMap = Map.Make (String)
module StringSet = Set.Make (String)

let parse_path p =
  let p = String.trim p in
  match String.split_on_char ~sep:' ' p with
  | [ origin; "to"; destination ] -> Some (origin, destination)
  | _ -> None

let parse_distance d = d |> String.trim |> int_of_string_opt

let parse_line str =
  match String.split_on_char ~sep:'=' str with
  | [ path; distance ] -> (
      match (parse_path path, parse_distance distance) with
      | Some (origin, destination), Some distance ->
          Some (origin, destination, distance)
      | _ -> None)
  | _ -> None

let add_entry map (origin, destination, distance) =
  let current_from_origin =
    StringMap.find_opt origin map |> Option.value ~default:[]
  in
  let current_from_destination =
    StringMap.find_opt destination map |> Option.value ~default:[]
  in
  map
  |> StringMap.add ~key:origin
       ~data:((destination, distance) :: current_from_origin)
  |> StringMap.add ~key:destination
       ~data:((origin, distance) :: current_from_destination)

let keys map = map |> StringMap.to_seq |> Seq.map fst |> StringSet.of_seq

let find_answer ~fold_init ~fold_f map =
  let find_answer' map key =
    let rec path acc key to_visit =
      if StringSet.is_empty to_visit then acc
      else
        map
        |> StringMap.find key
        |> List.filter ~f:(fun (dst, _) -> StringSet.mem dst to_visit)
        |> List.map ~f:(fun (destination, distance) ->
               path (acc + distance) destination
                 (StringSet.remove destination to_visit))
        |> List.fold_left ~init:fold_init ~f:fold_f
    in
    path 0 key (map |> keys |> StringSet.remove key)
  in

  StringMap.fold ~init:fold_init
    ~f:(fun ~key ~data:_ acc ->
      let shortest = find_answer' map key in
      fold_f acc shortest)
    map

let find_shortest = find_answer ~fold_init:max_int ~fold_f:min

let find_longest = find_answer ~fold_init:0 ~fold_f:max

let () =
  let map =
    Aoc.stdin
    |> Seq.filter_map parse_line
    |> Seq.fold_left add_entry StringMap.empty
  in
  Printf.printf "Part 1 (shortest): %d\n" (find_shortest map);
  Printf.printf "Part 2 (longest): %d\n" (find_longest map)
