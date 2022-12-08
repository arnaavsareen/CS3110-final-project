(*Game.ml defines functions that manages the game state and players*)
open Cards

(*player represents the player*)
type player = {
  name : string;
  hand : card list;
  money : int;
  bet : int;
}

(*pot represents the main pot with amount and side pot in side_pots*)
type pot = {
  amount : int;
  side_pots : int list;
}

(*Game_stage represnts the current stage of the game. Bet of int represents
  which player is betting right now.*)
type game_stage =
  | First_Bet of int
  | Flop
  | Second_Bet of int
  | Turn
  | Third_Bet of int
  | River
  | Final_bet of int
  | Finish

(* Game_state represesnts the state of the game, containing the stage, the
   players, *)
type game_state = {
  mutable stage : game_stage;
  mutable players : player list;
  mutable pot : pot;
  mutable deck : card list;
  mutable community_cards : card list;
  mutable current_bet : int;
}

val init_state : game_state

type deck = card list
type turn_order = player list

val next_turn : unit -> unit
val top_bet : turn_order -> int
val valid_bet : player -> turn_order -> int -> bool
val make_bet : player -> turn_order -> int -> player
val call_amount : player -> turn_order -> int
val is_side_pot : turn_order -> bool
val all_in : player -> bool
(*retruns a list of the vaious bet amounts of each sidepot ordered from lowest
  to highest*)

val side_pot_amount : turn_order -> int list
val side_pot_list : turn_order -> int list
val fix_values : int list -> int list