exception Basement of int

let proc acc = function
  | _, '(' -> acc + 1
  | idx, ')' -> if acc = 0 then raise_notrace (Basement idx) else acc - 1
  | _ -> acc

let () =
  try Aoc.stdin |> Seq.flat_map String.to_seqi |> Seq.fold_left proc 0 |> ignore
  with Basement idx -> Printf.printf "%d\n" (idx + 1)
