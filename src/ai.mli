(*controls the AI*)
open Cards
open Engine

(*Represents the decision an AI can make*)
type decision =
  | Fold
  | Check
  | Call
  | Raise

(*Make_decision makes a decision based on the current bet, cards in the
  community pool, and its own information*)
val make_decision : int -> card list -> decision