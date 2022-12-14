(*controls the AI*)
open Cards
open Engine

(*Represents the decision an AI can make*)
type decision =
  | Fold
  | Check
  | Call
  | Raise

(*Represents the probabilitys the AI is currently using. Fold x means that if a
  100 sided die rolls less than or equal to x, the ai folds. raise x means that
  if the ai want to raise, it will on average bet an average of 100*x percent of
  its money.*)
type percents = {
  fold : int;
  check : int;
  raise : float;
}

(*Make_decision makes a decision based on the current bet, cards in the
  community pool, and its own information*)
val make_decision : int -> card list -> player -> decision