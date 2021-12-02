module IntSet = Set.Make (struct
  type t = int * int

  let compare (x1, y1) (x2, y2) =
    let x = Int.compare x1 x2 in
    if x = 0 then Int.compare y1 y2 else x
end)

let split_seq seq =
  (* note: this is a hack because stdin would be consumed once. Can probably
     figure out a better way? idk *)
  let seq = seq |> List.of_seq |> List.to_seq in
  let s1 =
    Seq.filter_map (fun (idx, v) -> if idx mod 2 = 0 then Some v else None) seq
  in
  let s2 =
    Seq.filter_map (fun (idx, v) -> if idx mod 2 = 1 then Some v else None) seq
  in
  (s1, s2)

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
  let santa_seq, robot_seq =
    Aoc.stdin |> Seq.flat_map String.to_seqi |> split_seq
  in
  let { visited = santa_visited; _ } =
    Seq.fold_left proc initial_state santa_seq
  in
  let { visited = robot_visited; _ } =
    Seq.fold_left proc initial_state robot_seq
  in
  IntSet.union santa_visited robot_visited
  |> IntSet.cardinal
  |> Printf.printf "%d\n"
