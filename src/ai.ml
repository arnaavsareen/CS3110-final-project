(*Controls the ai*)
open Cards
open Engine

type decision =
  | Fold
  | Check
  | Call
  | Raise

let make_decision bet pool =
  let rando = Random.int 100 in
  if rando > 90 then Fold else if rando > 50 then Call else Raise
