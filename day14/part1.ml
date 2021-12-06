open StdLabels

type state = Flying | Resting

type reindeer = {
  name : string;
  speed : int;
  time_flying : int;
  time_resting : int;
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
      Some
        {
          name;
          speed = int_of_string speed;
          time_flying = int_of_string time_flying;
          time_resting = int_of_string time_resting;
        }
  | _ -> None

let distance_traveled ts { time_flying; time_resting; speed; _ } =
  let turn_duration = time_flying + time_resting in
  let baseline = ts / turn_duration * time_flying in
  let remainder = ts mod turn_duration in
  let seconds_flying = baseline + min remainder time_flying in
  seconds_flying * speed

let () =
  Aoc.stdin
  |> Seq.filter_map parse
  |> Seq.map (distance_traveled 2503)
  |> Seq.fold_left max 0
  |> Printf.printf "%d\n"
