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
