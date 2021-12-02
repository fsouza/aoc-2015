open StdLabels

let parse line =
  match String.split_on_char ~sep:'x' line with
  | [ x; y; z ] -> Some (int_of_string x, int_of_string y, int_of_string z)
  | _ -> None

let first_two = function
  | f :: s :: _ -> (f, s)
  | _ -> invalid_arg "need at least two elements"

let ribbon_needed (length, width, height) =
  let sides = List.sort ~cmp:Int.compare [ length; width; height ] in
  let smaller_perimiter =
    let f, s = first_two sides in
    (2 * f) + (2 * s)
  in
  (width * length * height) + smaller_perimiter

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.map ribbon_needed
  |> Seq.fold_left ( + ) 0
  |> Printf.printf "%d\n"
