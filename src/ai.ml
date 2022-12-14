(*Controls the ai*)
open Cards
open Engine

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

let low_roll = { fold = 50; check = 95; raise = 0.1 }
let avg_roll = { fold = 25; check = 80; raise = 0.25 }
let high_roll = { fold = 0; check = 50; raise = 0.75 }
let super_roll = { fold = 0; check = 50; raise = 1.0 }

let make_decision bet pool plyr =
  Unix.sleep 1;
  let rando = Random.int 100 in
  if rando > 70 then Fold else if rando > 20 then Call else Raise
