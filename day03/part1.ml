module IntSet = Set.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    let x = Int.compare x1 x2 in
    if x = 0 then Int.compare y1 y2 else x
end)

type state = { visited : IntSet.t; pos : int * int }

let initial_state = { visited = IntSet.empty; pos = (0, 0) }

let move (x, y) = function
  | '^' -> (x, y + 1)
  | '>' -> (x + 1, y)
  | 'v' -> (x, y - 1)
  | '<' -> (x - 1, y)
  | ch -> ch |> Printf.sprintf "unknown move: %c" |> invalid_arg

let proc { visited; pos } ch =
  let next_pos = move pos ch in
  { visited = visited |> IntSet.add pos |> IntSet.add next_pos; pos = next_pos }

let () =
  Aoc.stdin
  |> Seq.flat_map String.to_seq
  |> Seq.fold_left proc initial_state
  |> (fun { visited; _ } -> IntSet.cardinal visited)
  |> Printf.printf "%d\n"
