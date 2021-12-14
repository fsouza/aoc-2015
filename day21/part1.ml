open StdLabels
module IntSet = Set.Make (Int)

type item = { name : string; cost : int; damage : int; armor : int }

let all_weapons =
  [
    { name = "Dagger"; cost = 8; damage = 4; armor = 0 };
    { name = "Shortsword"; cost = 10; damage = 5; armor = 0 };
    { name = "Warhammer"; cost = 25; damage = 6; armor = 0 };
    { name = "Longsword"; cost = 40; damage = 7; armor = 0 };
    { name = "Greataxe"; cost = 74; damage = 8; armor = 0 };
  ]

let all_armors =
  [
    { name = "Nothing"; cost = 0; damage = 0; armor = 0 };
    { name = "Leather"; cost = 13; damage = 0; armor = 1 };
    { name = "Chainmail"; cost = 31; damage = 0; armor = 2 };
    { name = "Splintmail"; cost = 53; damage = 0; armor = 3 };
    { name = "Bandedmail"; cost = 75; damage = 0; armor = 4 };
    { name = "Platemail"; cost = 102; damage = 0; armor = 5 };
  ]

let rings =
  [|
    { name = "Nothing"; cost = 0; damage = 0; armor = 0 };
    { name = "Nothing"; cost = 0; damage = 0; armor = 0 };
    { name = "Damage +1"; cost = 25; damage = 1; armor = 0 };
    { name = "Damage +2"; cost = 50; damage = 2; armor = 0 };
    { name = "Damage +3"; cost = 100; damage = 3; armor = 0 };
    { name = "Defense +1"; cost = 20; damage = 0; armor = 1 };
    { name = "Defense +2"; cost = 40; damage = 0; armor = 2 };
    { name = "Defense +3"; cost = 80; damage = 0; armor = 3 };
  |]

type player = { damage : int; armor : int; items : item list }

let gold_spent { items; _ } =
  List.fold_left ~init:0 ~f:(fun acc { cost; _ } -> acc + cost) items

let delta_per_round player boss =
  let boss_damage = boss.damage - player.armor |> max 1 in
  let player_damage = player.damage - boss.armor |> max 1 in
  player_damage - boss_damage

let can_win_against boss player = delta_per_round player boss >= 0

let make_player items =
  let damage, armor =
    List.fold_left ~init:(0, 0)
      ~f:(fun (damage_acc, armor_acc) ({ damage; armor; _ } : item) ->
        (damage_acc + damage, armor_acc + armor))
      items
  in
  { damage; armor; items }

let all_players =
  let open Seq in
  let rec all_players' weapons armors ring_indices () =
    match (weapons, armors, ring_indices) with
    | [], _, _ -> Nil
    | _ :: tl, [], _ -> all_players' tl all_armors (0, 1) ()
    | weapons, armors, (i, j) when j = Array.length rings ->
        all_players' weapons armors (i + 1, i + 2) ()
    | weapons, _ :: tl, (i, _) when i = Array.length rings ->
        all_players' weapons tl (0, 1) ()
    | (weapon :: _ as weapons), (armor :: _ as armors), (i, j) ->
        let items = [ weapon; armor; rings.(i); rings.(j) ] in
        Cons (make_player items, all_players' weapons armors (i, j + 1))
  in
  all_players' all_weapons all_armors (0, 1)

let parse_boss =
  Seq.fold_left
    (fun boss line ->
      match String.split_on_char ~sep:':' line with
      | [ "Damage"; damage ] ->
          { boss with damage = damage |> String.trim |> int_of_string }
      | [ "Armor"; armor ] ->
          { boss with armor = armor |> String.trim |> int_of_string }
      | _ -> boss)
    { damage = 0; armor = 0; items = [] }

let print_player { damage; armor; items } =
  Printf.printf "damage: %d\n" damage;
  Printf.printf "armor: %d\n" armor;
  Printf.printf "items: %s\n"
    (List.map ~f:(fun { name; _ } -> name) items |> String.concat ~sep:",")

let print_player_more_details boss player =
  print_player player;
  Printf.printf "Delta: %d\n" @@ delta_per_round player boss;
  Printf.printf "Cost: %d\n" @@ gold_spent player

let solve boss =
  all_players
  |> Seq.filter (can_win_against boss)
  |> Seq.map gold_spent
  |> Seq.fold_left min max_int

let () = Aoc.stdin |> parse_boss |> solve |> Printf.printf "%d\n"
