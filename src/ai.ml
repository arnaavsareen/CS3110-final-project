(*Controls the ai*)
type decision =
  | Fold
  | Check
  | Raise of int

let make_decision bet pool plyer = Fold
