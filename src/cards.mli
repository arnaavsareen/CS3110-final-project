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
type number =
  | Number of int
      (**Card is a representation of a single card within a 52 card deck*)

type card = {
  color : color;
  suit : suit;
  number : number;
}

(*Deck is a represntation of a deck of cards*)
type deck = { mutable cards : card list }

(*Draws the top x cards, and crates a tuple (x,y), where x is the deck with top
  x cards removed and y is the drawn cards*)
(* val draw : int -> deck -> deck * deck *)

val suit_arr : suit array
(**suit and number array are used in constructing new cards and pattern matching*)

val number_arr : number array

(*Given a card, returns the suit of it in string form*)
val ascii_suit : suit -> string

(*given a card, returns the numbe rof it as a string**)
val str_number : number -> string

val shuffle : 'a array -> 'a
(**shuffles a deck of cards, given an array of cards*)

val deal : string

(* Returns a record with a card list field of a randomly shuffled 52 card deck*)
val init_shuffled_deck : card list

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