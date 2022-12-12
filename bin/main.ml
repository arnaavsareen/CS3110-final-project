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
  print_string "\nYou have four options -\n";
  (*print_string "1. To check, type in 'check'\n";*)
  print_string "1. To raise, type in 'raise'\n";
  print_string "2. To fold, type in 'fold'\n";
  print_string "3. To call, type in 'call'\n";
  print_string "4. To check, type in 'check'\n"

(*Bet call preforms a single bet for the player*)
let rec bet_call () =
  print_string "\n>";
  match read_line () with
  | "check" -> (
      try Engine.check state 0
      with e ->
        print_string
          "Sorry, that check is invalid, enter raise check or fold again: \n";
        bet_call ())
  | "raise" -> (
      print_string
        ("\nYou must raise by at least "
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

let rec execute_aidecision state p =
  let aidecision =
    Ai.make_decision state.current_bet (List.nth state.players 1).hand
  in
  match aidecision with
  | Fold ->
      print_string (p.name ^ " decided to fold\n\n");
      Engine.fold state p.position
  | Check -> ()
  | Raise -> (
      let r = Engine.top_bet state.players * 2 in
      try
        Engine.raise_bet state p.position r;

        print_string
          (p.name ^ " has raised by "
          ^ string_of_int (r - top_bet state.players)
          ^ "\n\n")
      with e -> execute_aidecision state p)
  | Call ->
      Engine.call state p.position;
      print_string
        (p.name ^ " has called, to put his bet at a total of "
        ^ string_of_int (List.nth state.players p.position).bet
        ^ "\n\n")

let ai_list plist =
  match plist with
  | h :: t -> t
  | _ -> failwith "unreachable"

let rec each_ai_turn state ai =
  match ai with
  | [] -> ()
  | h :: t ->
      if done_betting state then ()
      else if h.folded then (
        print_string ("\n" ^ h.name ^ " has folded so they do not get a turn\n");
        print_string "Enter any key to move to the next players turn";
        match read_line () with
        | exception End_of_file -> ()
        | _ -> each_ai_turn state t)
      else (
        print_string ("\n" ^ h.name ^ " is making their turn\n");
        execute_aidecision state h;
        print_string "Enter any key to move to the next players turn";
        match read_line () with
        | exception End_of_file -> ()
        | _ -> each_ai_turn state t)

let next_stage state =
  match state.stage with
  | Begin -> state.stage <- Flop
  | Flop -> state.stage <- Turn
  | Turn -> state.stage <- River
  | River -> state.stage <- Finish
  | _ -> ()

let tick_next state =
  next_stage state;
  update_pot state;
  reset_bets state;
  print_after_bet ();
  state.iterated <- false

let tick_helper state =
  print_dealer ();
  if done_betting state then () else bet_call ();
  each_ai_turn state (ai_list state.players);
  state.iterated <- true;
  if done_betting state then tick_next state

let rec tick2 () =
  match state.stage with
  | Begin ->
      tick_helper state;
      tick2 ()
  | Flop ->
      print_string hidden_flop3_str;
      tick_helper state;
      tick2 ()
  | Turn ->
      print_string hidden_flop4_str;
      tick_helper state;
      tick2 ()
  | River ->
      print_string flop_str;
      tick_helper state;
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

let make_player_list plist pnum =
  if pnum = 2 then Engine.make_player "TERA" 1 :: plist
  else if pnum = 3 then
    Engine.make_player "TERA" 1 :: Engine.make_player "Jerry" 2 :: plist
  else if pnum = 4 then
    Engine.make_player "TERA" 1
    :: Engine.make_player "Jerry" 2
    :: Engine.make_player "Michael Clarkson" 3
    :: plist
  else
    Engine.make_player "TERA" 1
    :: Engine.make_player "Jerry" 2
    :: Engine.make_player "Michael Clarkson" 3
    :: Engine.make_player "Clark Michaelson" 4
    :: plist

let rec playgame2 () =
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
      print_string "Enter the amount of players you want (Integer from 2-5)\n";
      match read_line () with
      | exception End_of_file -> ()
      | userchoice -> (
          let pnum = int_of_string userchoice in
          print_string "Enter your player name.\n";
          print_string
            "Example: Arnaav; Example Tyler; Example Ryan; Example: Eric\n";
          print_string "> ";
          match read_line () with
          | exception End_of_file -> ()
          | userchoice -> (
              let playername = userchoice ^ " " in
              let playerinputlst = String.split_on_char ' ' playername in
              let playerlst = List.filter (fun s -> s <> "") playerinputlst in
              let user = Engine.make_player playername 0 in
              let plist = make_player_list [ user ] pnum in
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
                  print_string
                    "\nOther players have also been dealt two cards.\n";

                  tick2 ()))
    end
  | "no" ->
      ANSITerminal.(
        print_string [ red ] "Quiting the game. See you next time :)")
  | _ -> (
      ANSITerminal.print_string [ ANSITerminal.red ] "\nWrong input!\n";
      ANSITerminal.print_string [ ANSITerminal.red ]
        "Press Q to Quit or P to Play Game.\n";
      ANSITerminal.print_string [ ANSITerminal.red ] "> ";
      match read_line () with
      | "Q" ->
          ANSITerminal.print_string [ ANSITerminal.red ]
            "Quiting the game. See you next time :"
      | "P" -> playgame2 ()
      | _ -> ())

let () = playgame2 ()