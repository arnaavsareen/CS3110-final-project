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

  (*Takes a list of 7 cards, and ordered them first according to multiplicity
    (How often they occur in the list) then by value. Returns an association
    list where every distinct card value is matched to it's multiplicity. For
    example : ordered_card_mult [king, ace, ace, four, four, seven, jack]
    returns [(2,ace);(2,four);(1,king);(1,jack);(1,seven)] Note: in this
    function we don't care about the suits of the cards*)
  val ordered_cards_mult : card list -> (number * int) list

  (*Takes a list of 7 cards as input, and returns a hand type of the best hand
    among those 7 cards*)
  val init_hand : card list -> t

  (*compare h1 h2 returns -1 if h1 is a worse hand than h2, 0 if the hands are
    equal (very rare) and 1 if h1 is a better hand than h2*)
  val compare : t -> t -> int

  (*Returns a string representation of the hand, with the type followed by the
    value of the cards representing that type*)
  val to_string : t -> string
end