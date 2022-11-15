(* [color] is the color of the card. It can be Red or Black. *)
type color =
  | Red
  | Black

(* [suit] is the type of a card suit. It can be Hearts, Spades, Diamonds,
   Clubs *)
type suit =
  | Hearts
  | Spades
  | Diamonds
  | Clubs

(* [number] is the rank of a card.*)
type number = Number of int

type card = {
  color : color;
  suit : suit;
  number : number;
}

val suit_arr : suit array
val number_arr : number array
val ascii_suit : suit -> string
val str_number : number -> string
val shuffle : 'a array -> 'a
val deal : string
val overturn_deal : string
val hidden_flop0_str : string
val hidden_flop1_str : string
val hidden_flop2_str : string
val flop_str : string

module Hand : sig
  (*The type of the hand is one of the poker hands, and represents set of 5
    cards that can be compared to others along usual poker rules*)
  type t

  (*Takes a list of 5 cards as input, and returns a hand type of those cards*)
  val init_hand : card list -> t

  (*compare h1 h2 returns -1 if h1 is a worse hand than h2, 0 if the hands are
    equal (very rare) and 1 if h1 is a better hand than h2*)
  val compare : t -> t -> int
end