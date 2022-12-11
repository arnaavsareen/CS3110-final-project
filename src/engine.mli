(*Game.ml defines functions that manages the game state and players*)
open Cards

type player = {
  name : string;
  hand : card list;
  money : int;
  bet : int;
}

type deck = card list
type turn_order = player list

val next_turn : unit -> unit
val top_bet : turn_order -> int
val valid_bet : player -> turn_order -> int -> bool
val make_bet : player -> turn_order -> int -> player
val call_amount : player -> turn_order -> int
val is_side_pot : turn_order -> bool
val all_in : player -> bool
val side_pot_amount : turn_order -> int list
val side_pot_list : turn_order -> int list
val fix_values : int list -> int list
val total_side_pot_player : turn_order -> int -> int list