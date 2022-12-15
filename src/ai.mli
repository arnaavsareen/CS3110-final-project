(** Module that implements the AI.

    This module Handles the decision making capabilities of our AI. *)

open Cards
open Engine

(** Represents the decision an AI can make.*)
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
(** Represents the percentage of the AI's decisions.*)

val make_decision : int -> card list -> player -> game_stage -> decision
(** [make_decision] makes a decision based on the current bet, cards in the
    community pool, and its own information.*)
