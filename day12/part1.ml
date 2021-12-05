open StdLabels

let sum_numbers : Yojson.Safe.t -> int =
 fun json ->
  let rec sum_numbers' acc = function
    | `Int i -> acc + i
    | `Assoc obj ->
        obj |> List.map ~f:snd |> List.fold_left ~init:acc ~f:sum_numbers'
    | `List list -> list |> List.fold_left ~init:acc ~f:sum_numbers'
    | _ -> acc
  in
  sum_numbers' 0 json

let () =
  read_line () |> Yojson.Safe.from_string |> sum_numbers |> Printf.printf "%d\n"
