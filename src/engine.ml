open Cards
open Stdlib

exception Impossible

type player = {
  name : string;
  hand : card list;
  money : int;
  bet : int;
  folded : bool;
}

type pot = {
  amount : int;
  side_pots : int list;
}

type options = { starting_money : int }

type game_stage =
  | Begin
  | First_Bet of int
  | Flop
  | Second_Bet of int
  | Turn
  | Third_Bet of int
  | River
  | Final_Bet of int
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
  mutable options : options;
}

let init_state =
  {
    stage = Begin;
    players = [];
    pot = { amount = 0; side_pots = [] };
    deck = [];
    community_cards = [];
    current_bet = 0;
    options = { starting_money = 0 };
  }

let advance_state x = ()
let make_player name = { name; hand = []; money = 500; bet = 0; folded = false }

let done_betting_helper playa current =
  if playa.money = 0 then true else playa.money = current

let done_betting status =
  if status.current_bet = 0 then false
  else
    List.fold_left
      (fun boolean playa ->
        boolean && done_betting_helper playa status.current_bet)
      true status.players

let increment status =
  let n = List.length status.players in
  match status.stage with
  | First_Bet x ->
      status.stage <- First_Bet ((x + 1) mod n);
      ()
  | Second_Bet x ->
      status.stage <- Second_Bet ((x + 1) mod n);
      ()
  | Third_Bet x ->
      status.stage <- Third_Bet ((x + 1) mod n);
      ()
  | Final_Bet x ->
      status.stage <- Final_Bet ((x + 1) mod n);
      ()
  | _ -> raise Impossible

let fold status = status.folded = true
let raise status amount = ()
let check status = ()

let string_of_player p =
  "Name = " ^ p.name ^ "Hand = hand " ^ "Money = " ^ string_of_int p.money
  ^ " Bet = " ^ string_of_int p.money

type deck = card list
type turn_order = player list
type list_int = int list

let next_turn x = ()

(*Returns the highest bet so far plist is a list of all the players in the
  hand*)
let rec top_bet plist =
  match plist with
  | [] -> 0
  | h :: t -> Stdlib.max h.bet (top_bet t)

(*checks to see if the bet is valid p is the player making bet plist is a list
  of all players in the hand b is an integer value of the bet amount*)
let valid_bet p plist b =
  if p.money = b then true (*all in*)
  else if b + p.bet < top_bet plist then false
  else if p.money < b + p.bet then false
  else true

(*if the bet is valid returns a player with bet and subtracts bet from money p
  is the player making bet otherwise returns original player plist is a list of
  all players in the hand b is an integer value of the bet amount*)
let make_bet p plist b =
  if valid_bet p plist b then
    {
      name = p.name;
      hand = p.hand;
      money = p.money - b;
      bet = p.bet + b;
      folded = p.folded;
    }
  else p

let set_bet state p b =
  if make_bet p state.players b != p then
    state.players <- make_bet p state.players b :: List.remove p state.players

(*retruns the amount it would take for the player to call*)
let call_amount p plist =
  if p.money + p.bet < top_bet plist then p.money else top_bet plist - p.bet

(*retruns true if there will be a side pot false otherwise*)
let rec is_side_pot plist =
  match plist with
  | h :: t -> if h.money = 0 && h.bet > 0 then true else is_side_pot t
  | _ -> false

(*returns true if the player has gone all in returns false otherwise*)
let all_in p = if p.money = 0 && p.bet > 0 then true else false

let rec fix_values x =
  match x with
  | [] -> []
  | [ h ] -> [ h ]
  | [ f; s ] -> [ f; s - f ]
  | [ f; s; t ] -> [ f; s - f; t - s ]
  | [ a; b; c; d ] -> [ a; b - a; c - b; d - c ]
  | [ a; b; c; d; e ] -> [ a; b - a; c - b; d - c; e - d ]
  | [ a; b; c; d; e; f ] -> [ a; b - a; c - b; d - c; e - d; f - e ]
  | [ a; b; c; d; e; f; g ] -> [ a; b - a; c - b; d - c; e - d; f - e; g - f ]
  | h :: t -> [ 696969 ]

let sort_list int_list = fix_values (List.sort compare int_list)

(*retruns a list of the vaious bet amounts of each sidepot ordered from lowest
  to highest*)
let rec side_pot_list plist =
  if is_side_pot plist then
    match plist with
    | [] -> []
    | h :: t ->
        if all_in h = true then sort_list (h.bet :: side_pot_list t)
        else side_pot_list t
  else []

let rec total_side_pot_player plist b =
  match plist with
  | [] -> []
  | h :: t ->
      if h.bet > b then b :: total_side_pot_player t b
      else total_side_pot_player t b

let rec sum_list l =
  match l with
  | [] -> 0
  | h :: t -> h + sum_list t

let rec total_pot_value plist =
  match plist with
  | [] -> 0
  | h :: t -> h.bet + total_pot_value t

let rec main_pot_amount plist =
  match plist with
  | [] -> 0
  | h :: t -> h.bet + main_pot_amount t

let set_pot state =
  state.pot <- { amount = main_pot_amount state.players; side_pots = [] }

(*{ amount = main_pot_amount plist; side_pots = [] }*)
(*testing*)
