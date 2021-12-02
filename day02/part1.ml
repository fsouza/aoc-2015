open StdLabels

let parse line =
  match String.split_on_char ~sep:'x' line with
  | [ x; y; z ] -> Some (int_of_string x, int_of_string y, int_of_string z)
  | _ -> None

let box_area (length, width, height) =
  let areas = [ length * width; width * height; length * height ] in
  let smallest_area = List.fold_left ~init:max_int ~f:min areas in
  areas
  |> List.fold_left ~init:0 ~f:(fun acc el -> acc + (el * 2))
  |> ( + ) smallest_area

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.map box_area
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "%d\n"
