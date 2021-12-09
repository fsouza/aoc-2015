open StdLabels

type state = Flying_until of int | Resting_until of int

type reindeer = {
  name : string;
  speed : int;
  time_flying : int;
  time_resting : int;
  state : state;
  score : int;
  distance_traveled : int;
}

let parse str =
  match String.split_on_char ~sep:' ' str with
  | [
   name;
   "can";
   "fly";
   speed;
   "km/s";
   "for";
   time_flying;
   "seconds,";
   "but";
   "then";
   "must";
   "rest";
   "for";
   time_resting;
   "seconds.";
  ] ->
      let time_flying = int_of_string time_flying in
      Some
        {
          name;
          speed = int_of_string speed;
          time_flying;
          time_resting = int_of_string time_resting;
          score = 0;
          state = Flying_until time_flying;
          distance_traveled = 0;
        }
  | _ -> None

let proc_reindeer ts
    ({ speed; state; time_flying; time_resting; distance_traveled; _ } as
    reindeer) =
  match state with
  | Flying_until t when ts < t ->
      { reindeer with distance_traveled = distance_traveled + speed }
  | Flying_until _ ->
      {
        reindeer with
        state = Resting_until (ts + time_resting);
        distance_traveled = distance_traveled + speed;
      }
  | Resting_until t when ts < t -> reindeer
  | Resting_until _ -> { reindeer with state = Flying_until (ts + time_flying) }

let add_score reindeers =
  reindeers
  |> Array.to_seqi
  |> Seq.fold_left
       (fun ((indices, leader) as acc) (i, { distance_traveled; _ }) ->
         if distance_traveled > leader then ([ i ], distance_traveled)
         else if distance_traveled < leader then acc
         else (i :: indices, distance_traveled))
       ([], 0)
  |> fst
  |> List.iter ~f:(fun leader_idx ->
         let ({ score; _ } as leader) = reindeers.(leader_idx) in
         reindeers.(leader_idx) <- { leader with score = score + 1 })

let run until reindeers =
  let rec run' ts =
    if ts = until + 1 then (
      add_score reindeers;
      Array.fold_left ~init:0
        ~f:(fun acc { score; _ } -> max acc score)
        reindeers)
    else (
      Array.iteri
        ~f:(fun i reindeer -> reindeers.(i) <- proc_reindeer ts reindeer)
        reindeers;
      add_score reindeers;
      run' (ts + 1))
  in
  run' 1

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Array.of_seq
  |> run 2503
  |> Printf.printf "%d\n"
