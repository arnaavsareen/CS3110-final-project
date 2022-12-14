(*Controls the ai*)
open Cards
open Engine
open Hand

type decision =
  | Fold
  | Check
  | Call
  | Raise

type percents = {
  fold : int;
  check : int;
  raise : float;
}

let instant_death = { fold = 100; check = 0; raise = 0.0 }
let low_roll = { fold = 50; check = 95; raise = 0.1 }
let avg_roll = { fold = 17; check = 85; raise = 0.25 }
let high_roll = { fold = 0; check = 70; raise = 0.75 }
let super_roll = { fold = 0; check = 50; raise = 1.0 }

let state_conversion = function
  | Pre_Flop -> 1
  | Flop -> 2
  | Turn -> 3
  | River -> 4
  | _ -> 5

let decision_helper state pool hand =
  let s = state_conversion state in
  if s = 1 then high_roll
  else
    let r = Cards.Hand.rank (Cards.Hand.init_hand (pool @ hand)) in
    if r > 5 then super_roll
    else if r > 3 then high_roll
    else if r > 1 then avg_roll
    else if s < 4 then avg_roll
    else low_roll

let downgrade roll =
  if roll = super_roll then high_roll
  else if roll = high_roll then avg_roll
  else if roll = avg_roll then low_roll
  else if roll = low_roll then instant_death
  else instant_death

let upgrade roll =
  if roll = super_roll then super_roll
  else if roll = high_roll then super_roll
  else if roll = avg_roll then high_roll
  else if roll = low_roll then avg_roll
  else low_roll

let final_modifier roll bet plyr =
  let b = float_of_int bet /. float_of_int plyr.money in
  if b > 0.7 then downgrade roll else if b > 0.15 then roll else upgrade roll

let make_decision bet pool plyr state =
  let rando = Random.int 100 in
  let prob = final_modifier (decision_helper state pool plyr.hand) bet plyr in
  if rando <= prob.fold then Fold
  else if rando <= prob.check then Call
  else Raise
