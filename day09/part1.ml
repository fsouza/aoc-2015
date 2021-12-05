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

let shortest_starting_at map key =
  let rec path acc key to_visit =
    if StringSet.is_empty to_visit then acc
    else
      map
      |> StringMap.find key
      |> List.filter ~f:(fun (dst, _) -> StringSet.mem dst to_visit)
      |> List.map ~f:(fun (destination, distance) ->
             path (acc + distance) destination
               (StringSet.remove destination to_visit))
      |> List.fold_left ~init:max_int ~f:min
  in
  path 0 key (map |> keys |> StringSet.remove key)

let find_shortest map =
  StringMap.fold ~init:max_int
    ~f:(fun ~key ~data:_ acc ->
      let shortest = shortest_starting_at map key in
      min acc shortest)
    map

let () =
  Aoc.stdin
  |> Seq.filter_map parse_line
  |> Seq.fold_left add_entry StringMap.empty
  |> find_shortest
  |> Printf.printf "%d\n"
