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