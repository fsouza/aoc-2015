open StdLabels
open MoreLabels
module IntSet = Set.Make (Int)
module StringMap = Map.Make (String)

let line_re = Re.compile (Re.Pcre.re {|^Sue (\d+):\s+(.+)$|})

let parse_fact fact =
  match String.split_on_char ~sep:':' fact with
  | [ compound; amount ] ->
      Some (String.trim compound, amount |> String.trim |> int_of_string)
  | _ -> None

let parse_facts facts =
  facts |> String.split_on_char ~sep:',' |> List.filter_map ~f:parse_fact

let parse line =
  line
  |> Re.exec_opt line_re
  |> Option.map Re.Group.all
  |> (Fun.flip Option.bind) (function
       | [| _; sue_number; facts |] ->
           Some (int_of_string sue_number, parse_facts facts)
       | _ -> None)

let tape =
  {|
children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1
|}
  |> String.split_on_char ~sep:'\n'
  |> List.to_seq
  |> Seq.filter (fun s -> String.length s > 0)
  |> Seq.filter_map parse_fact
  |> StringMap.of_seq

let cmp_fn = function
  | "cat" | "trees" -> ( > )
  | "pomeranians" | "goldfish" -> ( < )
  | _ -> ( = )

let all_sues =
  StringMap.fold ~init:IntSet.empty ~f:(fun ~key:_ ~data set ->
      data |> List.to_seq |> Seq.map fst |> (Fun.flip IntSet.add_seq) set)

let find_sue map =
  StringMap.fold ~init:(all_sues map)
    ~f:(fun ~key ~data set ->
      let cmp_fn = cmp_fn key in
      match StringMap.find_opt key map with
      | None | Some [] -> set
      | Some sues ->
          let sues_to_remove =
            sues
            |> List.to_seq
            |> Seq.filter_map (fun (sue_number, amount) ->
                   if cmp_fn amount data then None else Some sue_number)
            |> IntSet.of_seq
          in
          IntSet.diff set sues_to_remove)
    tape
  |> IntSet.min_elt

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.fold_left
       (fun map (sue_number, facts) ->
         List.fold_left ~init:map
           ~f:(fun map (compound, count) ->
             let curr =
               StringMap.find_opt compound map |> Option.value ~default:[]
             in
             StringMap.add ~key:compound ~data:((sue_number, count) :: curr) map)
           facts)
       StringMap.empty
  |> find_sue
  |> Printf.printf "%d\n"
