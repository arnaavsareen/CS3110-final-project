(* [color] is the color of the card. It can be Red or Black. *)
 type color = Red | Black

(* [suit] is the type of a card suit. It can be Hearts, Spades, Diamonds, 
  Clubs *)
 type suit = Hearts | Spades | Diamonds | Clubs
 
(* [number] is the rank of a card.*)
type number = Ace of bool | Number of int

val ascii_suit: suit -> string 

val str_number: number -> string 

val deal : string 

val overturn_deal : string 

val hidden_flop0 : string

val hidden_flop1 : string 

val hidden_flop2 : string 

val flop : string
