(*Controls the ai*)
open Cards
open Engine

type decision =
  | Fold
  | Check

let make_decision bet pool =
  let rando = Random.int 100 in
  if rando > 75 then Fold else Check
