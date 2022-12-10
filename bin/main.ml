open Game
open Author
open Cards
open Engine
open Ai

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

let ai_bet player_number =
  match
    Ai.make_decision state.current_bet state.community_cards
      (List.nth state.players player_number)
  with
  | Fold ->
      Engine.fold state player_number;
      ()
  | Check ->
      Engine.check state player_number;
      ()
  | Raise x ->
      Engine.raise state player_number x;
      ()

let rec bet_call () =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    ("Current Bet is " ^ string_of_int state.current_bet);
  print_string "Would you like to check, raise, or fold?";
  match String.lowercase_ascii (read_line ()) with
  | "check" -> Engine.check state 0
  | "raise" -> ()
  | "fold" -> Engine.fold state 0
  | _ ->
      print_string "Sorry, that is an invalid option. Please try again";
      bet_call ()

let flop_call x =
  Engine.flop state x;
  ()

let cleanup =
  Engine.cleanup state true;
  ()

let rec bet x =
  if Engine.done_betting state then ()
  else if x = 0 then (
    bet_call ();
    Engine.increment state)
  else ai_bet x;
  Engine.increment state

let rec tick () =
  match state.stage with
  | Begin ->
      select ();
      Engine.deal state 2;
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
      tick ()
  | Finish -> cleanup

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
  print_string "Note: No space after yes or no\n";
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
        "\nRequired number of players is atleast 1\n";
      print_string "Enter the names of all players with a space in between\n";
      print_string "Example: Arnaav Tyler Ryan Eric\n";
      print_string "> ";
      match read_line () with
      | exception End_of_file -> ()
      | userchoice -> (
          let players = String.split_on_char ' ' userchoice in
          let playerlst = List.filter (fun s -> s <> "") players in
          ANSITerminal.print_string [ ANSITerminal.magenta ] "\nWelcome - ";
          print_list playerlst;
          ANSITerminal.print_string [ ANSITerminal.magenta ]
            "\n\nLet's begin the game!\n";
          print_string "You have been dealt two cards: ";
          print_string "\n";
          print_string "nothing here yet";
          print_string "\nEnter any key to see your cards!\n";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | userchoice -> (
              print_string "\n";
              print_string overturn_deal;
              print_string "\nEnter any key to overturn your cards!\n";
              print_string "> ";
              match read_line () with
              | exception End_of_file -> ()
              | userchoice -> (
                  print_string "\n";
                  print_string "nothing here yet";
                  print_string "\n";
                  print_string "The flop is has been dealt.\n";
                  print_string "\n";
                  print_string hidden_flop0_str;
                  print_string "\n";
                  print_string "Enter any key to overturn your cards!\n";
                  print_string "> ";
                  match read_line () with
                  | exception End_of_file -> ()
                  | userchoice ->
                      print_string "\n";
                      print_string hidden_flop1_str;
                      print_string "\n";
                      print_string hidden_flop2_str;
                      print_string "\n";
                      print_string hidden_flop3_str;
                      print_string "\n";
                      print_string flop_str;
                      print_string "\n")))
    end
  | "no" ->
      ANSITerminal.(
        print_string [ red ] "Quiting the game. See you next time :))")
  | _ ->
      ANSITerminal.(
        print_string [ red ] "Wrong input. Try again!";
        playgame ())

let () = playgame ()