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
  (* | Number of int *)
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  | Ten
  | Eleven
  | Twelve
  | Thirteen
  | Fourteen

let ascii_suit = function
  | Spades -> "♠"
  | Diamonds -> "♦"
  | Hearts -> "♥"
  | Clubs -> "♣"

let str_number = function
  | Ace value -> if value then "1" else "14"
  (* |Number n -> str_of_int n *)
  | Two -> "2"
  | Three -> "3"
  | Four -> "4"
  | Five -> "5"
  | Six -> "6"
  | Seven -> "7"
  | Eight -> "8"
  | Nine -> "9"
  | Ten -> "10"
  | Eleven -> "J"
  | Twelve -> "Q"
  | Thirteen -> "K"
  | Fourteen -> "A"

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
  print_string (card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7)

let flop =
  let card1 = "│\t    ┌─────────┐ ┌─────────┐ \t\t\t\t│\n" in
  let card2 = "│\t    │         │ │         │ \t\t\t\t│\n" in
  let card3 = "│\t    │         │ │         │ \t\t\t\t│\n" in
  let card4 = "│\t    │         │ │         │ \t\t\t\t│\n" in
  let card5 = "│\t    │         │ │         │ \t\t\t\t│\n" in
  let card6 = "│\t    │         │ │         │ \t\t\t\t│\n" in
  let card7 = "│\t    └─────────┘ └─────────┘ \t\t\t\t│\n" in
  print_string (card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7)
