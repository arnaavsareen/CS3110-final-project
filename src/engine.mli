(** Module that defines functions to manage the game state and players.*)

open Cards

type player = {
  mutable name : string;
  mutable hand : card list;
  mutable money : int;
  mutable bet : int;
  mutable folded : bool;
  mutable position : int;
}
(** [player] represents the player in the game.*)

type pot = {
  amount : int;
  side_pots : int list;
}
(** [pot] represents the main pot with amount and side pot in side_pots.*)

type options = { starting_money : int }
(** [options] represents the various options one may select at the begining of
    the game.*)

(** [game_stage] represnts the current stage of the game. Bet of int represents
    which player is betting right now.*)
type game_stage =
  | Pre_Flop
  | Flop
  | Turn
  | River
  | Finish

type game_state = {
  mutable stage : game_stage;
  mutable players : player list;
  mutable pot : pot;
  mutable deck : card list;
  mutable community_cards : card list;
  mutable current_bet : int;
  mutable options : options;
  mutable iterated : bool;
  mutable rounds : int;
}
(** [game_state] represesnts the state of the game, containing the stage, the
    players. *)

type turn_order = player list
(** The order in which players play their turn.*)

val init_state : game_state
(** The initial state of the game.*)

val advance_state : game_state -> unit
(** Advances the game to another state.*)

val make_player : string -> int -> player
(** Makes a player.*)

val done_betting : game_state -> bool
(** Returns whether this game state is done betting or not.*)

val done_betting_help : turn_order -> bool
(** Helper function for the function done betting.*)

val muck_check : game_state -> bool
(** Retruns true if only 1 player is in the hand false otherwise.*)

val players_in_hand : game_state -> player list
(** Returns a list of the players in the hand.*)

(*Raise,fold, and check update game state as appropriately.*)

val raise_bet : game_state -> int -> int -> unit
(** Parameters: current gamestate , the player number, and the bet you are
    making. raise_bet: if the raise is valid it modifies the list of players
    removing the player at the player number index from the list and concats a
    new updated player with the bet changed to the proper amount and returns
    unit, if the bet is invalid then it raises the Invalid_Raise exception*)

val fold : game_state -> int -> unit
(** Parameters: current gamestate , the player number. fold: modifies the list
    of players removing the player at the player number index from the list and
    concats a new updated player with fold as true and returns unit*)

val check : game_state -> int -> unit
(** Perameters: current gamestate , player number. check: if the check is
    invalid then it raises the Invalid_Check exception otherwise it returns unit*)

val call : game_state -> int -> unit
(** Parameters: current gamestate , the player number, and the bet you are
    making. call: if the call is valid it modifies the list of players removing
    the player at the player number index from the list and concats a new
    updated player with the bet changed to the proper amount and returns unit,
    if the bet is invalid then it raises the Invalid_Call exception*)

val update_pot : game_state -> unit
(** Parameter: the current gamestate. update_pot: modifies the gamestate pot to
    be a new pot with the amount equal to the previous pot plus the sum of all
    the bets the players have made for that round and return unit*)

val next_turn : unit -> unit
(** Represents the next turn.*)

val top_bet : turn_order -> int
(** Iterates through the list of players and returns the highest bet*)

val valid_bet : player -> turn_order -> int -> bool
(** Checks to see if the bet is valid for the given player*)

val make_bet : player -> turn_order -> int -> player
(** If the bet is valid it returns a new player with the bet updated, otherwise
    it returns the original player*)

val call_amount : player -> turn_order -> int
(** Returns the amount a given player would need to bet to call a bet*)

val is_side_pot : turn_order -> bool
(** Retruns true if there will be a side pot false otherwise*)

val all_in : player -> bool
(** Retruns true if there player is all in false otherwise*)

val reset_bets : game_state -> unit
(** Resets all the bets in that round.*)

(* val set_bet : game_state -> player -> int -> unit *)
(*retruns a list of the vaious bet amounts of each sidepot ordered from lowest
  to highest*)

(*val side_pot_amount : turn_order -> int list*)

val bet_list : turn_order -> int list
(** Retruns a list of the vaious bet amounts of each sidepot ordered from lowest
    to highest*)

val fix_values : int list -> int list
(** Retruns thelist after fixing bet values*)

val draw_card : game_state -> card
(** [draw_card] draws the top card of the deck and mutates the deck to no longer
    contain that card*)

val deal_cards : game_state -> unit
(** [deal_card] deals a card from the deck and mutates the deck to no longer
    contain that card*)

val overturn_community_cards : game_state -> unit
(** [overturn_community_cards] over turns community card from the deck and
    mutates the sate of community cards in the deck*)

val players_in_pot_list : game_state -> player list list
(** [players_in_pot_list] takes in a game state and returns a list of player
    lists of the players who are in each pot, for example if there is a main pot
    and a side pot, it would return a list containing the list of players able
    to win the main pot as the first element and the list of players able to win
    the side pot as the second*)

val pot_amounts : turn_order -> int list -> int list
(** Returns a list of the various pot amounts in order of precidence*)

val next_game : game_state -> unit
(** Decrements the round and resets the game, but keeps everybodies money. if
    the rounds is at zero, returns false. If rounds are not, returns true.\*)
