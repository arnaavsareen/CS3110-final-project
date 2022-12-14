open Cards
open Stdlib

exception Impossible

type player = {
  mutable name : string;
  mutable hand : card list;
  mutable money : int;
  mutable bet : int;
  mutable folded : bool;
  mutable position : int;
}

type pot = {
  amount : int;
  side_pots : int list;
}

type options = { starting_money : int }

type game_stage =
  | Pre_Flop
  | Flop
  | Turn
  | River
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
  mutable iterated : bool;
  mutable rounds : int;
}

exception Invalid_Bet of string
exception Invalid_Check of string
exception Invalid_Call of string

let init_state =
  {
    stage = Pre_Flop;
    players = [];
    pot = { amount = 0; side_pots = [] };
    deck = init_shuffled_deck;
    community_cards = [];
    current_bet = 10;
    options = { starting_money = 0 };
    iterated = false;
    rounds = 4;
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
  if p.money + p.bet < top_bet plist then p.money
  else if p.bet = top_bet plist then 0
  else top_bet plist - p.bet

let rec done_betting_help plist =
  match plist with
  | [] -> true
  | h :: t ->
      if h.bet = top_bet plist then true && done_betting_help t
      else if h.folded = true then true && done_betting_help t
      else if h.money = 0 then true && done_betting_help t
      else false

let done_betting status =
  if status.iterated = false then false else done_betting_help status.players

let rec num_players_in_hand plist =
  match plist with
  | [] -> 0
  | h :: t ->
      if h.folded then num_players_in_hand t else 1 + num_players_in_hand t

let muck_check status =
  if num_players_in_hand status.players <= 1 then true else false

let rec players_in_hand_help list =
  match list with
  | [] -> []
  | h :: t ->
      if h.folded then players_in_hand_help t else h :: players_in_hand_help t

let players_in_hand state = players_in_hand_help state.players

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

let rec valid_check plist p = if p.bet = top_bet plist then true else false

let change_to_fold p =
  {
    name = p.name;
    hand = p.hand;
    money = p.money;
    bet = p.bet;
    folded = true;
    position = p.position;
  }

let fold status player_num =
  status.players <-
    sort_list
      (change_to_fold (List.nth status.players player_num)
      :: list_remove (List.nth status.players player_num) status.players)

let raise_bet status player_num amount =
  status.players <- sort_list status.players;
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
  if valid_check status.players (List.nth status.players player_num) then
    status.players <- sort_list status.players
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

let rec remove_dups list =
  match list with
  | [] -> []
  | h :: t -> if List.mem h t then remove_dups t else h :: remove_dups t

let sort_int_list list = List.sort compare (remove_dups list)

let rec bet_list plist =
  if is_side_pot plist then
    match plist with
    | [] -> []
    | h :: t -> sort_int_list (h.bet :: bet_list t)
  else []

let rec total_pot_value plist =
  match plist with
  | [] -> 0
  | h :: t -> h.bet + total_pot_value t

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

(*let rec print_bets plist = match plist with | [] -> "" | h :: t -> (h.name ^ "
  bet is " ^ string_of_int h.bet ^ "\n") ^ print_bets t*)

let rec reset_bets status =
  status.players <- sort_list (reset_helper status.players)

let rec main_pot_amount plist =
  match plist with
  | [] -> 0
  | h :: t -> h.bet + main_pot_amount t

let rec remove_pot_amount plist amount =
  match plist with
  | [] -> []
  | p :: t ->
      {
        name = p.name;
        bet = p.bet - amount;
        hand = p.hand;
        money = p.money;
        folded = p.folded;
        position = p.position;
      }
      :: remove_pot_amount t amount

let rec total_pot_amount plist amount =
  match plist with
  | [] -> 0
  | p :: t ->
      if p.bet - amount >= 0 then amount + total_pot_amount t amount
      else total_pot_amount t amount

let rec pot_amounts plist sp_list =
  match sp_list with
  | [] -> []
  | h :: t ->
      total_pot_amount plist h :: pot_amounts (remove_pot_amount plist h) t

let rec player_in_pot plist amount =
  match plist with
  | [] -> []
  | p :: t ->
      if p.bet - amount >= 0 && not p.folded then p :: player_in_pot t amount
      else player_in_pot t amount

let rec players_in_pots plist bet_list =
  match bet_list with
  | [] -> []
  | h :: t ->
      player_in_pot plist h :: players_in_pots (remove_pot_amount plist h) t

let players_in_pot_list state =
  players_in_pots state.players (bet_list state.players)

let get_head = function
  | h :: t -> h
  | _ -> failwith "has no elements"

let get_tail = function
  | h :: t -> t
  | _ -> failwith "has no elements"

let is_prev_sp state = if state.pot.side_pots = [] then false else true

let rec add_to_tail list a =
  match list with
  | [] -> []
  | h :: t ->
      if add_to_tail t a = [] then (h + a) :: add_to_tail t a
      else h :: add_to_tail t a

let update_pot state =
  if is_prev_sp state && is_side_pot state.players then
    let pot_list =
      pot_amounts state.players (fix_values (bet_list state.players))
    in
    state.pot <-
      {
        amount = state.pot.amount;
        side_pots =
          add_to_tail state.pot.side_pots (get_head pot_list)
          @ get_tail pot_list;
      }
  else if is_prev_sp state && not (is_side_pot state.players) then
    state.pot <-
      {
        amount = state.pot.amount;
        side_pots = add_to_tail state.pot.side_pots state.pot.amount;
      }
  else if is_side_pot state.players then
    let pot_list =
      pot_amounts state.players (fix_values (bet_list state.players))
    in
    state.pot <-
      {
        amount = state.pot.amount + get_head pot_list;
        side_pots = get_tail pot_list;
      }
  else
    state.pot <-
      {
        amount = state.pot.amount + main_pot_amount state.players;
        side_pots = [];
      }

let update_money_help2 p int_list = p.money <- p.money + get_head int_list

let rec update_money_help1 int_list plist =
  match plist with
  | [] -> ()
  | h :: t ->
      update_money_help2 h int_list;
      update_money_help1 (get_tail int_list) t

let update_money status plist =
  update_money_help1 (status.pot.amount :: status.pot.side_pots) plist

let draw_card state =
  let x = List.hd state.deck in
  state.deck <- List.filter (fun c -> c <> x) state.deck;
  x

let deal_cards state =
  state.players <-
    List.map
      (fun p -> { p with hand = [ draw_card state; draw_card state ] })
      state.players

let overturn_community_cards state =
  state.community_cards <-
    [
      draw_card state;
      draw_card state;
      draw_card state;
      draw_card state;
      draw_card state;
    ]

let player_reset plyr =
  {
    name = plyr.name;
    hand = [];
    money = plyr.money;
    bet = 0;
    folded = false;
    position = plyr.position;
  }

let next_game status =
  status.stage <- Pre_Flop;
  status.current_bet <- 0;
  status.community_cards <- [];
  status.iterated <- false;
  status.rounds <- status.rounds - 1;
  status.deck <- init_shuffled_deck;
  status.pot <- { amount = 0; side_pots = [] };
  status.players <- List.map player_reset status.players
