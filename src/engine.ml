open Cards

type player = {
  name : string;
  hand : card list;
  money : int;
  bet : int;
}

type deck = card list
type turn_order = player list

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
