let proc acc = function
  | '(' -> acc + 1
  | ')' -> acc - 1
  | _ -> acc

let () =
  Aoc.stdin
  |> Seq.flat_map String.to_seq
  |> Seq.fold_left proc 0
  |> Printf.printf "%d\n"
