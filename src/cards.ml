type color =
  | Red
  | Black

type suit =
  | Hearts
  | Spades
  | Diamonds
  | Clubs

type number =
  | Ace of bool
  | Number of int

let ascii_suit = function
  | Spades -> "♠"
  | Diamonds -> "♦"
  | Hearts -> "♥"
  | Clubs -> "♣"

let str_number = function
  | Ace value -> if value then "1" else "14"
  | Number 13 -> "Q"
  | Number 12 -> "K"
  | Number 11 -> "J"
  | Number n -> if n >= 2 then string_of_int n else "Not a valid card!"

let hidden_flop0 =
  let card1 = "│\t    ┌─────────┐ ┌─────────┐ ┌─────────┐\t\t\t\t│\n" in
  let card2 = "│\t    │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│\t\t\t\t│\n" in
  let card3 = "│\t    │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│\t\t\t\t│\n" in
  let card4 = "│\t    │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│\t\t\t\t│\n" in
  let card5 = "│\t    │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│\t\t\t\t│\n" in
  let card6 = "│\t    │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│\t\t\t\t│\n" in
  let card7 = "│\t    └─────────┘ └─────────┘ └─────────┘\t\t\t\t│\n" in
  print_string (card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7)

let hidden_flop1 =
  let card1 = "│\t    ┌─────────┐ ┌─────────┐ ┌─────────┐\t\t\t\t│\n" in
  let card2 = "│\t    │         │ │░░░░░░░░░│ │░░░░░░░░░│\t\t\t\t│\n" in
  let card3 = "│\t    │         │ │░░░░░░░░░│ │░░░░░░░░░│\t\t\t\t│\n" in
  let card4 = "│\t    │         │ │░░░░░░░░░│ │░░░░░░░░░│\t\t\t\t│\n" in
  let card5 = "│\t    │         │ │░░░░░░░░░│ │░░░░░░░░░│\t\t\t\t│\n" in
  let card6 = "│\t    │         │ │░░░░░░░░░│ │░░░░░░░░░│\t\t\t\t│\n" in
  let card7 = "│\t    └─────────┘ └─────────┘ └─────────┘\t\t\t\t│\n" in
  print_string (card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7)

let hidden_flop2 =
  let card1 = "│\t    ┌─────────┐ ┌─────────┐ ┌─────────┐\t\t\t\t│\n" in
  let card2 = "│\t    │         │ │         │ │░░░░░░░░░│\t\t\t\t│\n" in
  let card3 = "│\t    │         │ │         │ │░░░░░░░░░│\t\t\t\t│\n" in
  let card4 = "│\t    │         │ │         │ │░░░░░░░░░│\t\t\t\t│\n" in
  let card5 = "│\t    │         │ │         │ │░░░░░░░░░│\t\t\t\t│\n" in
  let card6 = "│\t    │         │ │         │ │░░░░░░░░░│\t\t\t\t│\n" in
  let card7 = "│\t    └─────────┘ └─────────┘ └─────────┘\t\t\t\t│\n" in
  print_string (card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7)

let flop =
  let card1 = "│\t    ┌─────────┐ ┌─────────┐ ┌─────────┐\t\t\t\t│\n" in
  let card2 = "│\t    │         │ │         │ │         │\t\t\t\t│\n" in
  let card3 = "│\t    │         │ │         │ │         │\t\t\t\t│\n" in
  let card4 = "│\t    │         │ │         │ │         │\t\t\t\t│\n" in
  let card5 = "│\t    │         │ │         │ │         │\t\t\t\t│\n" in
  let card6 = "│\t    │         │ │         │ │         │\t\t\t\t│\n" in
  let card7 = "│\t    └─────────┘ └─────────┘ └─────────┘\t\t\t\t│\n" in
  let card8 = "│\t    ┌─────────┐ ┌─────────┐ \t\t\t\t│\n" in
  let card9 = "│\t    │         │ │         │ \t\t\t\t│\n" in
  let card10 = "│\t   │         │ │         │ \t\t\t\t│\n" in
  let card11 = "│\t   │         │ │         │ \t\t\t\t│\n" in
  let card12 = "│\t   │         │ │         │ \t\t\t\t│\n" in
  let card13 = "│\t   │         │ │         │ \t\t\t\t│\n" in
  let card14 = "│\t   └─────────┘ └─────────┘ \t\t\t\t│\n" in
  print_string
    (card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ card8 ^ card9
   ^ card10 ^ card11 ^ card12 ^ card13 ^ card14)
