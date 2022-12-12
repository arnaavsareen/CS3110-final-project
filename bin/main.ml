open Game
open Author
open Cards
open Ai
open Engine
open Tableui
open Cards

exception Unimplemented of string

let rec print_list = function
  | [] -> ()
  | head :: tail ->
      print_string head;
      print_string " ";
      print_list tail

let state = Engine.init_state

let select () =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\nRequired number of players is atleast 1\n";
  print_string "Enter the names of all players with a space in between\n";
  print_string "Example: Arnaav Tyler Ryan Eric\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | userchoice ->
      (let players = String.split_on_char ' ' userchoice in
       let playerlst = List.filter (fun s -> s <> "") players in
       ANSITerminal.print_string [ ANSITerminal.magenta ] "\nWelcome - ";
       print_list playerlst;
       state.players <- List.map Engine.make_player playerlst;
       ANSITerminal.print_string [ ANSITerminal.magenta ]
         "\n\nLet's begin the game!\n");
      ()

(*flop_call draws x cards from the deck and adds them to the community pile. *)
let flop_call x =
  (* Engine.flop state x; *)
  ()

(*Cleanup advances state to the next game*)
let cleanup =
  (* Engine.cleanup state true; *)
  ()

(*Buy in for AI and player *)
let rec buyin_call p plist b = make_bet p plist 10

(*Prints what the dealer says to the player everytime before the player gets a
  chance to bet*)
let print_dealer () =
  print_string "\nYou have three options -\n";
  print_string "1. To check, type in 'check'\n";
  print_string "2. To raise, type in 'raise'\n";
  print_string "3. To fold, type in 'fold'\n";
  print_string "4. To call, type in 'call'\n"

(*Bet call preforms a single bet for the player*)
let rec bet_call () =
  print_string "\n>";
  match read_line () with
  | "check" -> (
      try Engine.check state 0
      with e ->
        print_string
          "Sorry, that bet is invalid, enter raise check or fold again: \n";
        bet_call ())
  | "raise" -> (
      print_string
        ("You must raise by at least "
        ^ string_of_int (2 * Engine.top_bet state.players)
        ^ "\n" ^ "Enter amount to raise by\n");
      print_string "> ";
      let amount = int_of_string (read_line ()) in
      try Engine.raise_bet state 0 amount
      with e ->
        print_string
          "Sorry, that bet is invalid, enter raise check or fold again: \n";
        bet_call ())
  | "fold" ->
      ANSITerminal.print_string [ ANSITerminal.green ] "\nTERA WINS!!!\n";
      exit 0
  | "call" -> Engine.call state 0
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ] "\nWrong input!";
      exit 0

let print_after_bet () =
  print_string
    ("\nYou have "
    ^ string_of_int (List.hd state.players).money
    ^ " money left" ^ "\n");
  print_string ("The total pot is: " ^ string_of_int state.pot.amount ^ "\n");
  ()

let ai_bet plyr =
  match Ai.make_decision state.current_bet state.community_cards with
  | Fold ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\n TERA FOLDED! \n  YOU WIN!!!\n";
      exit 0
  | Check ->
      Engine.check state 1;
      ()
  | _ -> failwith "todo"

(*Makes a bet, and checks if everyone is done betting.*)
let rec bet x =
  if Engine.done_betting state then ()
  else if x = 0 then (
    bet_call ();
    Engine.increment state)
  else ai_bet x;
  Engine.increment state

(* Tick is the function that advances the game state.e Tick calls functions that
   print out the gui and mutate state. Ticks calls itself once its done.*)
let rec tick () =
  match state.stage with
  | Begin ->
      select ();
      (* Engine.deal state 2; *)
      tick ()
  | First_Bet x ->
      bet_call ();
      state.stage <- Flop;
      tick ()
  | Flop -> flop_call 3
  | Second_Bet x ->
      bet_call ();
      state.stage <- Turn;
      tick ()
  | Turn -> flop_call 1
  | Third_Bet x ->
      bet_call ();
      state.stage <- River;
      tick ()
  | River -> flop_call 1
  | Final_Bet x ->
      bet_call ();
      state.stage <- Finish;
      (* Engine.deal state 2; *)
      tick ()
  | Finish -> failwith "sorry i forget what you had here deleted by accident"

let execute_aidecision state =
  let aidecision =
    Ai.make_decision state.current_bet (List.nth state.players 1).hand
  in
  match aidecision with
  | Fold ->
      print_string "\nTERA, folded, You win!";
      exit 0
  | Check -> ()
  | Raise ->
      let r = Engine.top_bet state.players * 2 in
      print_string ("TERA has raised by " ^ string_of_int r ^ "\n");
      Engine.raise_bet state 1 r
  | Call ->
      Engine.call state 1;
      print_string
        ("\nTera has called, to put his bet at a total of "
        ^ string_of_int (List.nth state.players 1).bet
        ^ "\n")

let rec tick2 () =
  match state.stage with
  | Begin ->
      bet_call ();
      execute_aidecision state;
      update_pot state;
      reset_bets state;
      print_after_bet ();
      state.stage <- Flop;
      tick2 ()
  | Flop ->
      print_string hidden_flop3_str;
      print_dealer ();

      bet_call ();
      execute_aidecision state;
      update_pot state;
      reset_bets state;
      print_after_bet ();
      state.stage <- Turn;
      tick2 ()
  | Turn ->
      print_string hidden_flop4_str;
      print_dealer ();

      bet_call ();
      execute_aidecision state;
      update_pot state;
      reset_bets state;
      print_after_bet ();
      state.stage <- River;
      tick2 ()
  | River ->
      print_string flop_str;
      print_dealer ();

      bet_call ();
      execute_aidecision state;
      update_pot state;
      reset_bets state;
      print_after_bet ();
      state.stage <- Finish;
      tick2 ()
  | Finish ->
      if List.length state.players = 1 then print_string "You win the round!"
      else
        (* let player_hand_cards = (List.hd state.players).hand in let
           ai_hand_cards = (List.nth state.players 1).hand in let ai_hand =
           Hand.init_hand (ai_hand_cards @ state.deck) in let player_hand =
           Hand.init_hand (player_hand_cards @ state.deck) in match Hand.compare
           ai_hand player_hand with | 0 -> print_string "It's a tie!!" | 1 ->
           print_string "TERA wins the round!!" | -1 -> print_string "You win
           the round!!" | _ -> failwith "not possible") *)
        print_string "Your hand was better! You win! \n"
  | _ -> failwith "should not occur"

let rec playgame () =
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
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | "yes" -> begin
      ANSITerminal.(
        print_string [ yellow ]
          "Let's start by reviewing the rules of our game!\n\
          \ \n\
           We are going to be playing Texas Hold'em Poker (the most popular of \
           all poker variations)\n\
          \ > In a game of Texas hold'em, each player is dealt two cards face \
           down (the 'hole cards')\n\
          \ > Over several betting rounds, five more cards are (eventually) \
           dealt face up in the middle of the table\n\
          \ > These face-up cards are called the 'community cards.' Each \
           player is free to use the community cards in combination with their \
           hole cards to build a five-card poker hand.\n\
          \ > Put simply, the person with the best hand wins. But there is a \
           lot of strategy involved to be good at poker. A well known \
           technique is 'bluffing'.\n\
           credits: https://www.pokernews.com/poker-rules/texas-holdem.html \n\n");

      ANSITerminal.print_string [ ANSITerminal.magenta ]
        "\n\
         This is a single player game where the user competes against an AI \
         named TERA.\n";
      print_string
        "Fun Fact: TERA is an accronym for Tyler Eric Ryan Arnaav, the members \
         of our group.\n";
      print_string "Enter your player name.\n";
      print_string
        "Example: Arnaav; Example Tyler; Example Ryan; Example: Eric\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | userchoice -> begin
          let playername = userchoice ^ " " in
          let playerinputlst = String.split_on_char ' ' playername in
          let playerlst = List.filter (fun s -> s <> "") playerinputlst in
          let user = Engine.make_player playername in
          let tera = Engine.make_player "TERA" in
          let plist = [ user; tera ] in
          state.players <- plist;
          ANSITerminal.print_string [ ANSITerminal.magenta ] "\nWelcome - ";
          print_list playerlst;
          ANSITerminal.print_string [ ANSITerminal.magenta ]
            "\n\nLet's begin the game!\n\n";
          (* print_string (table playername); *)
          print_string "\nBoth players have 500 chips each.\n";
          print_string "The buy in for each player is 10 chips.\n";
          print_string
            (playername ^ ", type 'buy in' to place your buy in bet!\n");
          print_string "> ";
          match read_line () with
          | "buy in" | "buy in " | " buy in" | " buy in " -> (
              let user_after_buyin = buyin_call user plist 10 in
              print_string
                ("You have "
                ^ string_of_int user_after_buyin.money
                ^ " chips left\n");
              let tera_after_buyin = buyin_call tera plist 10 in
              print_string
                ("TERA has "
                ^ string_of_int tera_after_buyin.money
                ^ " chips left\n");
              print_string "\n";
              print_string
                "Now that you have placed your buy in, you will be dealt 2 \
                 cards from the deck. \n";
              print_string "\n";
              print_string deal;
              print_string "\nEnter any key to overturn your cards!\n";
              print_string "> ";
              match read_line () with
              | userchoice -> (
                  print_string overturn_deal;
                  print_string
                    ("\nRemember your cards :))\n"
                   ^ "\nTERA has also been dealt two cards.\n"
                   ^ "It is time to bet now! You will go first.\n"
                   ^ "\nYou have three options -\n"
                   ^ "1. To check, type in 'check'\n"
                   ^ "2. To raise, type in 'raise'\n"
                   ^ "3. To fold, type in 'fold'\n" ^ "> ");
                  (match read_line () with
                  | "check" -> Engine.check state 0
                  | "raise" ->
                      print_string "Enter amount to raise by\n";
                      print_string "> ";
                      let amount = int_of_string (read_line ()) in
                      Engine.raise_bet state 0 amount
                  | "fold" ->
                      ANSITerminal.print_string [ ANSITerminal.green ]
                        "\nTERA WINS!!!\n";
                      exit 0
                  | _ ->
                      ANSITerminal.print_string [ ANSITerminal.red ]
                        "\nWrong input!";
                      exit 0);
                  print_string "\nNow it is TERA's turn to bet.\n";
                  (match
                     Ai.make_decision state.current_bet state.community_cards
                   with
                  | Fold ->
                      ANSITerminal.print_string [ ANSITerminal.green ]
                        "\n TERA FOLDED! \n  YOU WIN!!!\n";
                      exit 0
                  | Check ->
                      print_string "TERA checked.\n";
                      Engine.check state 1;
                      ()
                  | Raise ->
                      print_string "TERA raised by 100";
                      Engine.raise_bet state 1 100
                  | Call -> failwith "nothing");

                  print_string
                    "Now that both players have had a chance to bet, it is \
                     time for the flop.  \n";
                  print_string "\n";
                  print_string hidden_flop0_str;
                  print_string "\nEnter any key to overturn your cards!\n";
                  print_string "> ";
                  match read_line () with
                  | userchoice -> (
                      print_string hidden_flop1_str;
                      print_string hidden_flop2_str;
                      print_string hidden_flop3_str;
                      print_string
                        "\nNow it is time for another round of betting :))\n";
                      print_string "\nYou have three options -\n";
                      print_string "1. To check, type in 'check'\n";
                      print_string "2. To raise, type in 'raise'\n";
                      print_string "3. To fold, type in 'fold'\n";
                      print_string "> ";
                      (match read_line () with
                      | "check" -> Engine.check state 0
                      | "raise" ->
                          print_string "Enter amount to raise by\n";
                          print_string "> ";
                          let amount = int_of_string (read_line ()) in
                          Engine.raise_bet state 0 amount
                      | "fold" ->
                          ANSITerminal.print_string [ ANSITerminal.green ]
                            "\nTERA WINS!!!\n";
                          exit 0
                      | _ ->
                          ANSITerminal.print_string [ ANSITerminal.red ]
                            "\nWrong input!";
                          exit 0);
                      print_string "\nNow it is TERA's turn to bet.\n";
                      (match
                         Ai.make_decision state.current_bet
                           state.community_cards
                       with
                      | Fold ->
                          ANSITerminal.print_string [ ANSITerminal.green ]
                            "\n TERA FOLDED! \n  YOU WIN!!!\n";
                          exit 0
                      | Check ->
                          print_string "TERA checked.\n";
                          Engine.check state 1
                      | Raise ->
                          print_string "TERA raised by 100";
                          Engine.raise_bet state 1 100
                      | Call -> failwith "nothing");
                      print_string
                        "Now that both players have had a chance to bet, it is \
                         time for the river.\n";
                      print_string "\n";
                      print_string flop_str;
                      print_string
                        "\n\
                         Enter your best hand consisting of 5 cards from the \
                         river and your deal in the following format.";
                      print_string
                        "\n\
                         1. Royal_Flush (*Royal flush always wins, unless it \
                         is dealt by the flop*)\n\
                         2. Straight_Flush of number (*highest card*)\n\
                         3. Four_of_a_kind of number * number (*four single*)\n\
                         4. Full_house of number * number (*triple double*)\n\
                         5. Flush of number * number * number * number * \
                         number (*descending value*)\n\
                         6. Straight of number (*highest card*)\n\
                         7. Three_of_a_kind of number * number * number (**)\n\
                         8. Two_pairs of number * number * number \n\
                         9. Pair of number * number * number * number\n\
                         10. High_card of number * number * number * number * \
                         number\n";
                      print_string "> ";
                      match read_line () with
                      | userchoice ->
                          ANSITerminal.print_string [ ANSITerminal.green ]
                            "\nYOU WIN!\nLETS GOOOOOOOOOOOOOOO\n")))
          | _ ->
              ANSITerminal.print_string [ ANSITerminal.red ] "\nWrong input!\n"
        end
    end
  | "no" ->
      ANSITerminal.(
        print_string [ red ] "Quiting the game. See you next time :))")
  | _ -> (
      ANSITerminal.print_string [ ANSITerminal.red ] "\nWrong input!\n";
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Press Q to Quit or P to Play Game.\n";
      ANSITerminal.print_string [ ANSITerminal.red ] "> ";
      match read_line () with
      | "Q" ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "Quiting the game. See you next time :))"
      | "P" -> playgame ()
      | _ -> ())

let rec playgame2 () =
  print_string "Enter your player name.\n";
  print_string "Example: Arnaav; Example Tyler; Example Ryan; Example: Eric\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | userchoice -> (
      let playername = userchoice ^ " " in
      let playerinputlst = String.split_on_char ' ' playername in
      let playerlst = List.filter (fun s -> s <> "") playerinputlst in
      let user = Engine.make_player playername in
      let tera = Engine.make_player "TERA" in
      let plist = [ user; tera ] in
      state.players <- plist;
      ANSITerminal.print_string [ ANSITerminal.magenta ] "\nWelcome - ";
      print_list playerlst;
      ANSITerminal.print_string [ ANSITerminal.magenta ]
        "\n\nLet's begin the game!\n\n";
      (* print_string (table playername); *)
      print_string "\nBoth players have 500 chips each.\n";
      print_string deal;
      print_string "\nEnter any key to overturn your cards!\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> failwith "error"
      | userchoice ->
          print_string overturn_deal;
          print_string "\nRemember your cards :))\n";
          print_string "\nTERA has also been dealt two cards.\n";
          print_dealer ();

          tick2 ())

let () = playgame2 ()