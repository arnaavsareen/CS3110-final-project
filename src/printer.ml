open Cards

let print_intro () =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    " \n\
    \     /$$$$$$   /$$$$$$   /$$$$$$  /$$$$$$ /$$   /$$  /$$$$$$       \n\
    \    /$$__  $$ /$$__  $$ /$$__  $$|_  $$_/| $$$ | $$ /$$__  $$      \n\
    \   | $$    _/| $$    $$| $$    _/  | $$  | $$$$| $$| $$    $$      \n\
    \   | $$      | $$$$$$$$|  $$$$$$   | $$  | $$ $$ $$| $$  | $$      \n\
    \   | $$      | $$__  $$   ___  $$  | $$  | $$  $$$$| $$  | $$      \n\
    \   | $$    $$| $$  | $$ /$$    $$  | $$  | $$   $$$| $$  | $$      \n\
    \   |  $$$$$$/| $$  | $$|  $$$$$$/ /$$$$$$| $$    $$|  $$$$$$/      \n\
    \      _____/ |__/  |__/   _____/ |______/|__/    _/   _____/       \n\
    \                                                                   \n\
    \                                                                   \n\
    \                                                                   ";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    " \n\
    \     /$$$$$$  /$$$$$$        /$$$$$$$                         \n\
    \    /$$__  $$/$$__  $$      | $$ __ $$                        \n\
    \   | $$    _/ $$    _/      | $$    $$ / $$  / $$ /$$$$$$/$$$$\n\
    \   | $$     |  $$$$$$       | $$$$$$$ | $$ | $$|  $$ _ $$ _ $$\n\
    \   | $$        ___  $$      | $$ __ $$| $$ | $$|  $$   $$   $$\n\
    \   | $$    $$/$$   $$       | $$   $$ | $$ | $$|  $$ | $$ | $$\n\
    \   |  $$$$$$/  $$$$$$/      | $$$$$$$/| $$$$$$ /| $$ | $$ | $$\n\
    \   |_______/   _____/       |_______/    _____/ |__/ |__/ |__/\n\
    \                                                              \n\
    \                                                              \n\
    \                                                              ";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "\n\
     CS 3110 Final Project by Arnaav Sareen, Tyler Forstrom, Eric Yang, and \
     Ryan Noonan\n";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\nAre you ready to play some poker today? (yes/no)\n";
  print_string "Note: Do not enter a space after 'yes' or 'no'.\n";
  print_string "> "

let print_rules () =
  ANSITerminal.(
    print_string [ yellow ]
      "Let's start by reviewing the rules of our game!\n\
      \ \n\
       We are going to be playing Texas Hold'em Poker (the most popular of all \
       poker variations)\n\
      \ > In a game of Texas hold'em, each player is dealt two cards face down \
       (the 'hole cards')\n\
      \ > Over several betting rounds, five more cards are (eventually) dealt \
       face up in the middle of the table\n\
      \ > These face-up cards are called the 'community cards.' Each player is \
       free to use the community cards in combination with their hole cards to \
       build a five-card poker hand.\n\
      \ > Put simply, the person with the best hand wins. But there is a lot \
       of strategy involved to be good at poker. A well known technique is \
       'bluffing'.\n\
       credits: https://www.pokernews.com/poker-rules/texas-holdem.html \n\n");

  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\
     This is a single player game where the user competes against an AI named \
     TERA.\n";
  print_string
    "Fun Fact: TERA is an accronym for Tyler Eric Ryan Arnaav, the members of \
     our group.\n";
  print_string "Enter the amount of players you want (Integer from 2-5)\n>"

let deal =
  let card1 = "      ┌─────────┐ ┌─────────┐         \n" in
  let card2 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card3 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card4 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card5 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card6 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card7 = "      └─────────┘ └─────────┘         \n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let print_deal () = print_string deal

let two_card_string cards =
  let c1 = List.hd cards in
  let c2 = List.nth cards 1 in
  let num1 = str_number c1.number in
  let suit1 = ascii_suit c1.suit in
  let num2 = str_number c2.number in
  let suit2 = ascii_suit c2.suit in
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

let three_card_string cards =
  let c1 = List.hd cards in
  let c2 = List.nth cards 1 in
  let c3 = List.nth cards 2 in
  let num1 = str_number c1.number in
  let suit1 = ascii_suit c1.suit in
  let num2 = str_number c2.number in
  let suit2 = ascii_suit c2.suit in
  let num3 = str_number c3.number in
  let suit3 = ascii_suit c3.suit in
  let head1 = " ----------------------------------------------- \n" in
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐      │\n" in
  let card2 =
    "│      │ " ^ num1 ^ "       │ │ " ^ num2 ^ "       │ │ " ^ num3
    ^ "       │      │\n"
  in
  let card3 = "│      │         │ │         │ │         │      │\n" in
  let card4 =
    "│      │    " ^ suit1 ^ "    │ │    " ^ suit2 ^ "    │ │    " ^ suit3
    ^ "    │      │\n"
  in
  let card5 = "│      │         │ │         │ │         │      │\n" in
  let card6 = "│      │         │ │         │ │         │      │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘      │\n" in
  let tail1 = " ----------------------------------------------- \n" in
  head1 ^ card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ tail1

let four_card_string cards =
  let c1 = List.hd cards in
  let c2 = List.nth cards 1 in
  let c3 = List.nth cards 2 in
  let c4 = List.nth cards 3 in
  let num1 = str_number c1.number in
  let suit1 = ascii_suit c1.suit in
  let num2 = str_number c2.number in
  let suit2 = ascii_suit c2.suit in
  let num3 = str_number c3.number in
  let suit3 = ascii_suit c3.suit in
  let num4 = str_number c4.number in
  let suit4 = ascii_suit c4.suit in
  let head1 = " ----------------------------------------------- \n" in
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐      │\n" in
  let card2 =
    "│      │ " ^ num1 ^ "       │ │ " ^ num2 ^ "       │ │ " ^ num3
    ^ "       │      │\n"
  in
  let card3 = "│      │         │ │         │ │         │      │\n" in
  let card4 =
    "│      │    " ^ suit1 ^ "    │ │    " ^ suit2 ^ "    │ │    " ^ suit3
    ^ "    │      │\n"
  in
  let card5 = "│      │         │ │         │ │         │      │\n" in
  let card6 = "│      │         │ │         │ │         │      │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘      │\n" in
  let card11 = "│      ┌─────────┐                              │\n" in
  let card12 =
    "│      │ " ^ num4 ^ "       │                              │\n"
  in
  let card13 = "│      │         │                              │\n" in
  let card14 =
    "│      │    " ^ suit4 ^ "    │                              │\n"
  in
  let card15 = "│      │         │                              │\n" in
  let card16 = "│      │         │                              │\n" in
  let card17 = "│      └─────────┘                              │\n" in
  let tail1 = " ----------------------------------------------- \n" in
  head1 ^ card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ card11
  ^ card12 ^ card13 ^ card14 ^ card15 ^ card16 ^ card17 ^ tail1

let five_card_string cards =
  let c1 = List.hd cards in
  let c2 = List.nth cards 1 in
  let c3 = List.nth cards 2 in
  let c4 = List.nth cards 3 in
  let c5 = List.nth cards 4 in
  let num1 = str_number c1.number in
  let suit1 = ascii_suit c1.suit in
  let num2 = str_number c2.number in
  let suit2 = ascii_suit c2.suit in
  let num3 = str_number c3.number in
  let suit3 = ascii_suit c3.suit in
  let num4 = str_number c4.number in
  let suit4 = ascii_suit c4.suit in
  let num5 = str_number c5.number in
  let suit5 = ascii_suit c5.suit in
  let head1 = " ----------------------------------------------- \n" in
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐      │\n" in
  let card2 =
    "│      │ " ^ num1 ^ "       │ │ " ^ num2 ^ "       │ │ " ^ num3
    ^ "       │      │\n"
  in
  let card3 = "│      │         │ │         │ │         │      │\n" in
  let card4 =
    "│      │    " ^ suit1 ^ "    │ │    " ^ suit2 ^ "    │ │    " ^ suit3
    ^ "    │      │\n"
  in
  let card5 = "│      │         │ │         │ │         │      │\n" in
  let card6 = "│      │         │ │         │ │         │      │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘      │\n" in
  let card11 = "│      ┌─────────┐ ┌─────────┐                  │\n" in
  let card12 =
    "│      │ " ^ num4 ^ "       │ │ " ^ num5 ^ "       │                  │\n"
  in
  let card13 = "│      │         │ │         │                  │\n" in
  let card14 =
    "│      │    " ^ suit4 ^ "    │ │    " ^ suit5
    ^ "    │                  │\n"
  in
  let card15 = "│      │         │ │         │                  │\n" in
  let card16 = "│      │         │ │         │                  │\n" in
  let card17 = "│      └─────────┘ └─────────┘                  │\n" in
  let tail1 = " ----------------------------------------------- \n" in
  head1 ^ card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ card11
  ^ card12 ^ card13 ^ card14 ^ card15 ^ card16 ^ card17 ^ tail1

let print_cards cards =
  match List.length cards with
  | 2 -> print_string (two_card_string cards)
  | 3 -> print_string (three_card_string cards)
  | 4 -> print_string (four_card_string cards)
  | 5 -> print_string (five_card_string cards)
  | _ -> failwith "should not occur"

(* let overturn_deal = let num1 = str_number (shuffle number_arr) in let suit1 =
   ascii_suit (shuffle suit_arr) in let num2 = str_number (shuffle number_arr)
   in let suit2 = ascii_suit (shuffle suit_arr) in let card1 = " ┌─────────┐
   ┌─────────┐ \n" in let card2 = " │ " ^ num1 ^ " │ │ " ^ num2 ^ " │ \n" in let
   card3 = " │ │ │ │ \n" in let card4 = " │ " ^ suit1 ^ " │ │ " ^ suit2 ^ " │
   \n" in let card5 = " │ │ │ │ \n" in let card6 = " │ │ │ │ \n" in let card7 =
   " └─────────┘ └─────────┘ \n" in card1 ^ card2 ^ card3 ^ card4 ^ card5 ^
   card6 ^ card7

   let hidden_flop0_str = let head1 = "
   ----------------------------------------------- \n" in let card1 = "│
   ┌─────────┐ ┌─────────┐ ┌─────────┐ │\n" in let card2 = "│ │░░░░░░░░░│
   │░░░░░░░░░│ │░░░░░░░░░│ │\n" in let card3 = "│ │░░░░░░░░░│ │░░░░░░░░░│
   │░░░░░░░░░│ │\n" in let card4 = "│ │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│ │\n"
   in let card5 = "│ │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│ │\n" in let card6 = "│
   │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│ │\n" in let card7 = "│ └─────────┘
   └─────────┘ └─────────┘ │\n" in let tail1 = "
   ----------------------------------------------- \n" in head1 ^ card1 ^ card2
   ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ tail1

   let hidden_flop1_str = let head1 = "
   ----------------------------------------------- \n" in let card1 = "│
   ┌─────────┐ ┌─────────┐ ┌─────────┐ │\n" in let card2 = "│ │ " ^ num1 ^ " │
   │░░░░░░░░░│ │░░░░░░░░░│ │\n" in let card3 = "│ │ │ │░░░░░░░░░│ │░░░░░░░░░│
   │\n" in let card4 = "│ │ " ^ suit1 ^ " │ │░░░░░░░░░│ │░░░░░░░░░│ │\n" in let
   card5 = "│ │ │ │░░░░░░░░░│ │░░░░░░░░░│ │\n" in let card6 = "│ │ │ │░░░░░░░░░│
   │░░░░░░░░░│ │\n" in let card7 = "│ └─────────┘ └─────────┘ └─────────┘ │\n"
   in let tail1 = " ----------------------------------------------- \n" in head1
   ^ card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ tail1

   let hidden_flop2_str = let head1 = "
   ----------------------------------------------- \n" in let card1 = "│
   ┌─────────┐ ┌─────────┐ ┌─────────┐ │\n" in let card2 = "│ │ " ^ num1 ^ " │ │
   " ^ num2 ^ " │ │░░░░░░░░░│ │\n" in let card3 = "│ │ │ │ │ │░░░░░░░░░│ │\n" in
   let card4 = "│ │ " ^ suit1 ^ " │ │ " ^ suit2 ^ " │ │░░░░░░░░░│ │\n" in let
   card5 = "│ │ │ │ │ │░░░░░░░░░│ │\n" in let card6 = "│ │ │ │ │ │░░░░░░░░░│
   │\n" in let card7 = "│ └─────────┘ └─────────┘ └─────────┘ │\n" in let tail1
   = " ----------------------------------------------- \n" in head1 ^ card1 ^
   card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ tail1

   let hidden_flop3_str = let head1 = "
   ----------------------------------------------- \n" in let card1 = "│
   ┌─────────┐ ┌─────────┐ ┌─────────┐ │\n" in let card2 = "│ │ " ^ num1 ^ " │ │
   " ^ num2 ^ " │ │ " ^ num3 ^ " │ │\n" in let card3 = "│ │ │ │ │ │ │ │\n" in
   let card4 = "│ │ " ^ suit1 ^ " │ │ " ^ suit2 ^ " │ │ " ^ suit3 ^ " │ │\n" in
   let card5 = "│ │ │ │ │ │ │ │\n" in let card6 = "│ │ │ │ │ │ │ │\n" in let
   card7 = "│ └─────────┘ └─────────┘ └─────────┘ │\n" in let tail1 = "
   ----------------------------------------------- \n" in head1 ^ card1 ^ card2
   ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ tail1

   let hidden_flop4_str = let head1 = "
   ----------------------------------------------- \n" in let card1 = "│
   ┌─────────┐ ┌─────────┐ ┌─────────┐ │\n" in let card2 = "│ │ " ^ num1 ^ " │ │
   " ^ num2 ^ " │ │ " ^ num3 ^ " │ │\n" in let card3 = "│ │ │ │ │ │ │ │\n" in
   let card4 = "│ │ " ^ suit1 ^ " │ │ " ^ suit2 ^ " │ │ " ^ suit3 ^ " │ │\n" in
   let card5 = "│ │ │ │ │ │ │ │\n" in let card6 = "│ │ │ │ │ │ │ │\n" in let
   card7 = "│ └─────────┘ └─────────┘ └─────────┘ │\n" in let card11 = "│
   ┌─────────┐ │\n" in let card12 = "│ │ " ^ num4 ^ " │ │\n" in let card13 = "│
   │ │ │\n" in let card14 = "│ │ " ^ suit4 ^ " │ │\n" in let card15 = "│ │ │
   │\n" in let card16 = "│ │ │ │\n" in let card17 = "│ └─────────┘ │\n" in let
   tail1 = " ----------------------------------------------- \n" in head1 ^
   card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ card11 ^ card12 ^
   card13 ^ card14 ^ card15 ^ card16 ^ card17 ^ tail1

   let flop_str = let head1 = " -----------------------------------------------
   \n" in let card1 = "│ ┌─────────┐ ┌─────────┐ ┌─────────┐ │\n" in let card2 =
   "│ │ " ^ num1 ^ " │ │ " ^ num2 ^ " │ │ " ^ num3 ^ " │ │\n" in let card3 = "│
   │ │ │ │ │ │ │\n" in let card4 = "│ │ " ^ suit1 ^ " │ │ " ^ suit2 ^ " │ │ " ^
   suit3 ^ " │ │\n" in let card5 = "│ │ │ │ │ │ │ │\n" in let card6 = "│ │ │ │ │
   │ │ │\n" in let card7 = "│ └─────────┘ └─────────┘ └─────────┘ │\n" in let
   card11 = "│ ┌─────────┐ ┌─────────┐ │\n" in let card12 = "│ │ " ^ num4 ^ " │
   │ " ^ num5 ^ " │ │\n" in let card13 = "│ │ │ │ │ │\n" in let card14 = "│ │ "
   ^ suit4 ^ " │ │ " ^ suit5 ^ " │ │\n" in let card15 = "│ │ │ │ │ │\n" in let
   card16 = "│ │ │ │ │ │\n" in let card17 = "│ └─────────┘ └─────────┘ │\n" in
   let tail1 = " ----------------------------------------------- \n" in head1 ^
   card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ card11 ^ card12 ^
   card13 ^ card14 ^ card15 ^ card16 ^ card17 ^ tail1 *)
