open StdLabels
open MoreLabels
module IntSet = Set.Make (Int)

type spell = Magic_missile | Drain | Shield | Poison | Recharge

let spell_cost = function
  | Magic_missile -> 53
  | Drain -> 73
  | Shield -> 113
  | Poison -> 173
  | Recharge -> 229

let string_of_spell = function
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

let cast (player, boss) spell =
  match spell with
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

let attack attacker defender =
  if attacker.id = "Boss" then
    let damage = attacker.damage - defender.armor |> max 1 in
    (attacker, { defender with hp = defender.hp - damage })
  else (attacker, defender)

(* this isn't great *)
let smallest = ref max_int

let record_and_return ({ mana_spent; id; _ } as winner) =
  if id = "Player" && mana_spent < !smallest then smallest := mana_spent;
  winner

let rec play attacker defender =
  let attacker_effects =
    attacker.effects |> List.filter_map ~f:process_effect
  in
  let defender_effects =
    defender.effects |> List.filter_map ~f:process_effect
  in
  let attacker = { attacker with effects = attacker_effects } in
  let defender = { defender with effects = defender_effects } in
  let attacker, defender =
    attacker_effects
    |> List.fold_left ~init:(attacker, defender) ~f:apply_effect
  in
  let defender, attacker =
    defender_effects
    |> List.fold_left ~init:(defender, attacker) ~f:apply_effect
  in
  let spells = available_spells all_spells attacker in
  if lost attacker then record_and_return defender
  else if lost defender then record_and_return attacker
  else if attacker.id = "Player" && (not @@ has_enough_mana spells attacker)
  then record_and_return defender
  else
    let attacker, defender = attack attacker defender in
    if lost defender then attacker
    else if attacker.id = "Player" then
      SpellSet.fold ~init:[]
        ~f:(fun spell acc ->
          let attacker, defender = cast (attacker, defender) spell in
          let spell_cost = spell_cost spell in
          let mana_spent = attacker.mana_spent + spell_cost in
          if mana_spent > !smallest then acc
          else
            play defender
              {
                attacker with
                mana_spent;
                current_mana = attacker.current_mana - spell_cost;
              }
            :: acc)
        spells
      |> List.filter ~f:(fun player -> player.id = "Player")
      |> List.fold_left ~init:{ initial_player with mana_spent = max_int }
           ~f:(fun acc player ->
             if player.mana_spent < acc.mana_spent then player else acc)
    else play { defender with armor = 0 } { attacker with armor = 0 }

let play player boss =
  play player boss |> fun { mana_spent; _ } -> Printf.printf "%d\n" mana_spent

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
