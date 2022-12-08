open Cards

type player = {
  name : string;
  hand : card list;
  money : int;
  bet : int;
}

type pot = {
  amount : int;
  side_pots : int list;
}

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
    { name = p.name; hand = p.hand; money = p.money - b; bet = p.bet + b }
  else p

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

let rec side_pot_amount plist =
  match plist with
  | [] -> []
  | h :: t -> []

let rec main_pot_amount plist =
  match plist with
  | [] -> 0
  | h :: t -> h.bet + main_pot_amount t
