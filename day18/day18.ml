open StdLabels
open MoreLabels

module PosSet = Set.Make (struct
  type t = int * int

  let compare (row1, col1) (row2, col2) =
    let row = Int.compare row1 row2 in
    if row = 0 then Int.compare col1 col2 else row
end)

type grid = { n : int; on : PosSet.t }

let add_row_to_grid { on; _ } (row_idx, row) =
  let on =
    row
    |> String.to_seqi
    |> Seq.fold_left
         (fun grid (col_idx, ch) ->
           match ch with
           | '#' -> PosSet.add (row_idx, col_idx) grid
           | _ -> grid)
         on
  in
  { n = String.length row; on }

let neighbors (row, col) n =
  [
    (row - 1, col - 1);
    (row - 1, col);
    (row - 1, col + 1);
    (row, col - 1);
    (row, col + 1);
    (row + 1, col - 1);
    (row + 1, col);
    (row + 1, col + 1);
  ]
  |> List.filter ~f:(fun (row, col) ->
         row > -1 && row < n && col > -1 && col < n)

let step pinned { on = old_grid; n } =
  let rec step' to_visit visited new_grid =
    match to_visit with
    | [] -> new_grid
    | pos :: rest when PosSet.mem pos visited -> step' rest visited new_grid
    | pos :: rest ->
        let is_on = PosSet.mem pos old_grid in
        let neighbors = neighbors pos n in
        let neighbors_to_visit =
          neighbors |> List.filter ~f:(fun pos -> not (PosSet.mem pos visited))
        in
        let to_visit = rest @ neighbors_to_visit in
        let on_neighbors =
          neighbors
          |> List.filter ~f:(fun pos -> PosSet.mem pos old_grid)
          |> List.length
        in
        let visited = PosSet.add pos visited in
        if is_on && (on_neighbors = 2 || on_neighbors = 3) then
          step' to_visit visited (PosSet.add pos new_grid)
        else if (not is_on) && on_neighbors = 3 then
          step' to_visit visited (PosSet.add pos new_grid)
        else step' to_visit visited new_grid
  in
  let new_grid =
    step' (PosSet.to_seq old_grid |> List.of_seq) PosSet.empty pinned
  in
  { n; on = new_grid }

let rec run steps pinned grid =
  let grid = { grid with on = PosSet.union grid.on pinned } in
  if steps = 0 then grid else run (steps - 1) pinned (step pinned grid)

let make_corners n =
  [ (0, 0); (0, n - 1); (n - 1, 0); (n - 1, n - 1) ] |> PosSet.of_list

let () =
  let steps = Sys.argv.(1) |> int_of_string in
  let grid =
    Aoc.zip Aoc.nat Aoc.stdin
    |> Seq.fold_left add_row_to_grid { n = 0; on = PosSet.empty }
  in
  grid
  |> run steps PosSet.empty
  |> (fun { on; _ } -> PosSet.cardinal on)
  |> Printf.printf "Part 1: %d\n";
  grid
  |> run steps (make_corners grid.n)
  |> (fun { on; _ } -> PosSet.cardinal on)
  |> Printf.printf "Part 2: %d\n"
