open Random

type color =
  | Red
  | Black

type suit =
  | Hearts
  | Spades
  | Diamonds
  | Clubs

type number = Number of int

type card = {
  color : color;
  suit : suit;
  number : number;
}

let suit_arr = [| Hearts; Spades; Diamonds; Clubs |]

let number_arr =
  [|
    Number 1;
    Number 2;
    Number 3;
    Number 4;
    Number 5;
    Number 6;
    Number 7;
    Number 8;
    Number 9;
    Number 10;
    Number 11;
    Number 12;
    Number 13;
    Number 14;
  |]

let ascii_suit = function
  | Spades -> "♠"
  | Diamonds -> "♦"
  | Hearts -> "♥"
  | Clubs -> "♣"

let str_number = function
  | Number 14 -> "A"
  | Number 13 -> "Q"
  | Number 12 -> "K"
  | Number 11 -> "J"
  | Number n -> if n >= 2 then string_of_int n else "Not a valid card!"

let shuffle arr =
  Random.self_init ();
  Array.get arr (Random.int (Array.length arr))

let deal =
  let card1 = "      ┌─────────┐ ┌─────────┐         \n" in
  let card2 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card3 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card4 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card5 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card6 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card7 = "      └─────────┘ └─────────┘         \n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let overturn_deal =
  let num1 = str_number (shuffle number_arr) in
  let suit1 = ascii_suit (shuffle suit_arr) in
  let num2 = str_number (shuffle number_arr) in
  let suit2 = ascii_suit (shuffle suit_arr) in
  let card1 = "      ┌─────────┐ ┌─────────┐         \n" in
  let card2 =
    "      │ " ^ num1 ^ "       │ │ " ^ num2 ^ "       │         \n"
  in
  let card3 = "      │         │ │         │         \n" in
  let card4 =
    "      │    " ^ suit1 ^ "    │ │    " ^ suit2 ^ "    │         \n"
  in
  let card5 = "      │         │ │         │         \n" in
  let card6 = "      │         │ │         │         \n" in
  let card7 = "      └─────────┘ └─────────┘         \n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let hidden_flop0 =
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐        │\n" in
  let card2 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card3 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card4 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card5 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card6 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘        │\n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let hidden_flop1 =
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐        │\n" in
  let card2 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card3 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card4 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card5 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card6 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘        │\n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let hidden_flop2 =
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐        │\n" in
  let card2 = "│      │         │ │         │ │░░░░░░░░░│        │\n" in
  let card3 = "│      │         │ │         │ │░░░░░░░░░│        │\n" in
  let card4 = "│      │         │ │         │ │░░░░░░░░░│        │\n" in
  let card5 = "│      │         │ │         │ │░░░░░░░░░│        │\n" in
  let card6 = "│      │         │ │         │ │░░░░░░░░░│        │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘        │\n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let hidden_flop3 =
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐        │\n" in
  let card2 = "│      │         │ │         │ │         │        │\n" in
  let card3 = "│      │         │ │         │ │         │        │\n" in
  let card4 = "│      │         │ │         │ │         │        │\n" in
  let card5 = "│      │         │ │         │ │         │        │\n" in
  let card6 = "│      │         │ │         │ │         │        │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘        │\n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let flop =
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐        │\n" in
  let card2 = "│      │         │ │         │ │         │        │\n" in
  let card3 = "│      │         │ │         │ │         │        │\n" in
  let card4 = "│      │         │ │         │ │         │        │\n" in
  let card5 = "│      │         │ │         │ │         │        │\n" in
  let card6 = "│      │         │ │         │ │         │        │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘        │\n" in
  let card8 = "│      ┌─────────┐ ┌─────────┐                    │\n" in
  let card9 = "│      │         │ │         │                    │\n" in
  let card10 = "│     │         │ │         │                    │\n" in
  let card11 = "│     │         │ │         │                    │\n" in
  let card12 = "│     │         │ │         │                    │\n" in
  let card13 = "│     │         │ │         │                    │\n" in
  let card14 = "│     └─────────┘ └─────────┘                    │\n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ card8 ^ card9 ^ card10
  ^ card11 ^ card12 ^ card13 ^ card14
