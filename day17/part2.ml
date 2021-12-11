let solve liters containers =
  let rec solve' target used_containers containers =
    if target = 0 then (1, used_containers)
    else if target < 0 then (0, 0)
    else
      match containers with
      | [] -> (0, 0)
      | hd :: tl ->
          let total_taking, used_taking =
            solve' (target - hd) (used_containers + 1) tl
          in
          let total_skipping, used_skipping =
            solve' target used_containers tl
          in
          if used_taking < used_skipping then (total_taking, used_taking)
          else if used_taking > used_skipping then
            (total_skipping, used_skipping)
          else (total_taking + total_skipping, used_skipping)
  in
  solve' liters max_int containers |> fst

let () =
  let liters = Sys.argv.(1) |> int_of_string in
  Aoc.stdin
  |> Seq.map int_of_string
  |> List.of_seq
  |> solve liters
  |> Printf.printf "%d\n"
