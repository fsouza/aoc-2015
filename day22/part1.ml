open StdLabels
open MoreLabels
module IntSet = Set.Make (Int)

type spell = Magic_missile | Drain | Shield | Poison | Recharge | No_spell

let spell_cost = function
  | No_spell -> 0
  | Magic_missile -> 53
  | Drain -> 73
  | Shield -> 113
  | Poison -> 173
  | Recharge -> 229

let string_of_spell = function
  | No_spell -> "no spell"
  | Magic_missile -> "Magic missile"
  | Drain -> "Drain"
  | Shield -> "Shield"
  | Poison -> "Poison"
  | Recharge -> "Recharge"

module SpellSet = Set.Make (struct
  type t = spell

  let compare s1 s2 =
    let cost1 = spell_cost s1 in
    let cost2 = spell_cost s2 in
    Int.compare cost1 cost2
end)

let all_spells =
  [ Magic_missile; Drain; Shield; Poison; Recharge ] |> SpellSet.of_list

type effect =
  | Boost_armor of int * int
  | Damage of int * int
  | Add_mana of int * int

type player = {
  id : string;
  hp : int;
  damage : int;
  armor : int;
  current_mana : int;
  mana_spent : int;
  effects : effect list;
}

let available_spells spells { effects; _ } =
  effects
  |> List.fold_left ~init:spells ~f:(fun spells -> function
       | Boost_armor (_, _) -> SpellSet.remove Shield spells
       | Damage (_, _) -> SpellSet.remove Poison spells
       | Add_mana (_, _) -> SpellSet.remove Recharge spells)

let initial_player =
  {
    id = "Player";
    hp = 50;
    damage = 0;
    armor = 0;
    current_mana = 500;
    mana_spent = 0;
    effects = [];
  }

let cast (player, boss) = function
  | No_spell -> (player, boss)
  | Magic_missile -> (player, { boss with hp = boss.hp - 4 })
  | Drain -> ({ player with hp = player.hp + 2 }, { boss with hp = boss.hp - 2 })
  | Shield ->
      ({ player with effects = Boost_armor (7, 6) :: player.effects }, boss)
  | Poison -> ({ player with effects = Damage (3, 6) :: player.effects }, boss)
  | Recharge ->
      ({ player with effects = Add_mana (101, 5) :: player.effects }, boss)

let apply_effect (owner, other) = function
  | Boost_armor (boost, _) -> ({ owner with armor = owner.armor + boost }, other)
  | Damage (damage, _) -> (owner, { other with hp = other.hp - damage })
  | Add_mana (mana, _) ->
      ({ owner with current_mana = owner.current_mana + mana }, other)

let process_effect = function
  | Boost_armor (_, 0) -> None
  | Damage (_, 0) -> None
  | Add_mana (_, 0) -> None
  | Boost_armor (boost, n) -> Some (Boost_armor (boost, n - 1))
  | Damage (damage, n) -> Some (Damage (damage, n - 1))
  | Add_mana (mana, n) -> Some (Add_mana (mana, n - 1))

let lost { hp; _ } = hp <= 0

let has_enough_mana spells { current_mana; _ } =
  let cheapest_spell = spells |> SpellSet.min_elt |> spell_cost in
  current_mana >= cheapest_spell

let attack spells attacker defender =
  if attacker.id = "Boss" then (
    let damage = attacker.damage - defender.armor |> max 1 in
    Printf.printf "Boss attacks for %d damage!\n" damage;
    (attacker, { defender with hp = defender.hp - damage }, No_spell))
  else
    let spell =
      SpellSet.find_last
        ~f:(fun spell -> spell_cost spell <= attacker.current_mana)
        spells
    in
    let attacker, defender = cast (attacker, defender) spell in
    Printf.printf "Player casts %s.\n" @@ string_of_spell spell;
    (attacker, defender, spell)

let rec play spells attacker defender =
  Printf.printf "-- %s turn --\n" attacker.id;
  let attacker_effects =
    attacker.effects |> List.filter_map ~f:process_effect
  in
  let defender_effects =
    defender.effects |> List.filter_map ~f:process_effect
  in
  let attacker = { attacker with effects = attacker_effects } in
  let defender = { defender with effects = defender_effects } in
  Printf.printf "- %s has %d hit points, %d armor, %d mana\n" attacker.id
    attacker.hp attacker.armor attacker.current_mana;
  Printf.printf "- %s has %d hit points, %d armor, %d mana\n" defender.id
    defender.hp defender.armor defender.current_mana;
  let attacker, defender =
    attacker_effects
    |> List.fold_left ~init:(attacker, defender) ~f:apply_effect
  in
  let defender, attacker =
    defender_effects
    |> List.fold_left ~init:(defender, attacker) ~f:apply_effect
  in
  let spells = available_spells spells attacker in
  if lost attacker then defender
  else if lost defender then attacker
  else if attacker.id = "Player" && (not @@ has_enough_mana spells attacker)
  then defender
  else
    let attacker, defender, spell_used = attack spells attacker defender in
    let spell_cost = spell_cost spell_used in
    if lost defender then attacker
    else (
      print_newline ();
      play spells
        { defender with armor = 0 }
        {
          attacker with
          armor = 0;
          mana_spent = attacker.mana_spent + spell_cost;
          current_mana = attacker.current_mana - spell_cost;
        })

let play player boss =
  play all_spells player boss |> fun { id; _ } -> Printf.printf "%s won!\n" id

let parse_boss =
  Seq.fold_left
    (fun boss line ->
      match String.split_on_char ~sep:':' line with
      | [ "Hit Points"; hp ] ->
          { boss with hp = hp |> String.trim |> int_of_string }
      | [ "Damage"; damage ] ->
          { boss with damage = damage |> String.trim |> int_of_string }
      | _ -> boss)
    { initial_player with hp = 0; id = "Boss"; current_mana = 0 }

let () =
  let boss = Aoc.stdin |> parse_boss in
  play initial_player boss
