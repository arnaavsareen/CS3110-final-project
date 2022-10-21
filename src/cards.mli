(* [color] is the color of the card. It can be Red or Black. *)
type color = Red | Black

(* [suit] is the type of a card suit. It can be Hearts, Spades, Diamonds, 
  Clubs *)
type suit = Hearts | Spades | Diamonds | Clubs
 
(* [number] is the rank of a card.*)
type number = Number of int

type card = {
  color : color;
  suit : suit;
  number : number;
}

val suit_list: suit list

val number_list: number list

val ascii_suit: suit -> string 

val str_number: number -> string 

val shuffle: 'a list -> 'a

val deal : string 

val overturn_deal : string 

val hidden_flop0 : string

val hidden_flop1 : string 

val hidden_flop2 : string 

val flop : string
