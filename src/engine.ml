open Cards
open Stdlib

exception Impossible

type player = {
  name : string;
  hand : card list;
  money : int;
  bet : int;
  folded : bool;
  position : int;
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

exception Invalid_Bet of string
exception Invalid_Check of string
exception Invalid_Call of string

let init_state =
  {
    stage = Begin;
    players = [];
    pot = { amount = 0; side_pots = [] };
    deck = [];
    community_cards = [];
    current_bet = 10;
    options = { starting_money = 0 };
  }

let advance_state x = ()

let sort_list plist =
  List.sort
    (fun a b ->
      if a.position > b.position then 1
      else if a.position > b.position then -1
      else 0)
    plist

let make_player name pos =
  { name; hand = []; money = 500; bet = 0; folded = false; position = pos }

let rec top_bet plist =
  match plist with
  | [] -> 0
  | h :: t -> Stdlib.max h.bet (top_bet t)

let call_amount p plist =
  if p.money + p.bet < top_bet plist then p.money else top_bet plist - p.bet

let rec done_betting_help plist =
  match plist with
  | [] -> true
  | h :: t ->
      if
        (h.bet = call_amount h plist || h.folded = true || h.money = 0)
        && done_betting_help t
      then true
      else false

let done_betting status = done_betting_help status.players

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

let valid_bet p plist b =
  if p.money = b then true (*all in*)
  else if b + p.bet < top_bet plist then false
  else if p.money < b + p.bet then false
  else if p.bet + b > top_bet plist && p.bet + b < 2 * top_bet plist then false
  else true

let make_bet p plist b =
  if valid_bet p plist b then
    {
      name = p.name;
      hand = p.hand;
      money = p.money - b;
      bet = p.bet + b;
      folded = p.folded;
      position = p.position;
    }
  else p

let rec list_remove x list =
  match list with
  | [] -> []
  | h :: t -> if h = x then list_remove x t else h :: list_remove x t

let rec valid_check plist =
  match plist with
  | [] -> true
  | h :: t -> if h.bet > 0 then false else valid_check t

let change_to_fold p =
  {
    name = p.name;
    hand = p.hand;
    money = p.money;
    bet = 0;
    folded = true;
    position = p.position;
  }

let fold status player_num =
  status.players <-
    sort_list
      (change_to_fold (List.nth status.players player_num)
      :: list_remove (List.nth status.players player_num) status.players)

let raise_bet status player_num amount =
  let p = List.nth status.players player_num in
  if make_bet p status.players amount != p then
    status.players <-
      sort_list
        (make_bet p status.players amount :: list_remove p status.players)
  else raise (Invalid_Bet "Invalid bet")

let call status player_num =
  let p = List.nth status.players player_num in
  let call_am = call_amount p status.players in
  status.players <-
    sort_list (make_bet p status.players call_am :: list_remove p status.players)

let check status player_num =
  if valid_check status.players then ()
  else raise (Invalid_Check "invalid check")

let deal status cards = status cards
let flop status cards = status cards
let cleanup status bool = status bool

let string_of_player p =
  "Name = " ^ p.name ^ "Hand = hand " ^ "Money = " ^ string_of_int p.money
  ^ " Bet = " ^ string_of_int p.money

type deck = card list
type turn_order = player list
type list_int = int list

let next_turn x = ()

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

(*retruns a list of the vaious bet amounts of each sidepot ordered from lowest
  to highest*)

let sort_int_list list = List.sort compare list

let rec side_pot_list plist =
  if is_side_pot plist then
    match plist with
    | [] -> []
    | h :: t ->
        if all_in h = true then sort_int_list (h.bet :: side_pot_list t)
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

let rec total_bet_amount plist =
  match plist with
  | [] -> 0
  | h :: t -> h.bet + total_bet_amount t

let rec reset_helper plist =
  match plist with
  | [] -> []
  | p :: t ->
      {
        name = p.name;
        hand = p.hand;
        money = p.money;
        bet = 0;
        folded = p.folded;
        position = p.position;
      }
      :: reset_helper t
(* let rec print_bets plist = match plist with | [] -> "" | h :: t -> (h.name ^
   " bet is " ^ string_of_int h.bet ^ "\n") ^ print_bets t*)

let rec reset_bets status = status.players <- reset_helper status.players

let update_pot state =
  state.pot <-
    {
      amount = state.pot.amount + total_bet_amount state.players;
      side_pots = [];
    }

(* let set_bet = () *)

(*{ amount = main_pot_amount plist; side_pots = [] }*)
(*testing*)
