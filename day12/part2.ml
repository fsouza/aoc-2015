open StdLabels

let is_red : Yojson.Safe.t -> bool = function
  | `String s -> String.equal s "red"
  | _ -> false

let sum_numbers : Yojson.Safe.t -> int =
 fun json ->
  let rec sum_numbers' acc = function
    | `Int i -> acc + i
    | `Assoc obj ->
        if List.exists ~f:(fun (_, value) -> is_red value) obj then acc
        else obj |> List.map ~f:snd |> List.fold_left ~init:acc ~f:sum_numbers'
    | `List list -> list |> List.fold_left ~init:acc ~f:sum_numbers'
    | _ -> acc
  in
  sum_numbers' 0 json

let () =
  read_line () |> Yojson.Safe.from_string |> sum_numbers |> Printf.printf "%d\n"
