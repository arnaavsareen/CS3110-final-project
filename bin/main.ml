open Game
open Author
open Cards
open Ai
open Engine
open Tableui
open Cards
open Printer

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
  | "fold" -> Engine.fold state 0
  | "call" -> Engine.call state 0
  | _ ->
      ANSITerminal.print_string [ ANSITerminal.red ] "Wrong input!";
      bet_call ()

let print_after_bet () =
  print_string
    ("\nYou have "
    ^ string_of_int (List.hd state.players).money
    ^ " money left" ^ "\n");
  print_string ("The total pot is: " ^ string_of_int state.pot.amount ^ "\n");
  ()

let ai_bet plyr =
  match Ai.make_decision state.current_bet state.community_cards plyr with
  | Fold ->
      ANSITerminal.print_string [ ANSITerminal.green ]
        "\n TERA FOLDED! \n  YOU WIN!!!\n";
      exit 0
  | Check ->
      Engine.check state 1;
      ()
  | _ -> failwith "todo"

(*Makes a bet, and checks if everyone is done betting.*)

let rec execute_aidecision state p =
  let aidecision =
    Ai.make_decision state.current_bet (List.nth state.players 1).hand p
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

        print_string (p.name ^ " has raised to " ^ string_of_int r ^ "\n\n")
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
      if done_betting state || muck_check state then ()
      else if h.folded then (
        print_string
          ("\n" ^ h.name ^ " has folded so they do not get a turn\n\n");
        print_string "Enter any key to move to the next players turn\n>";
        match read_line () with
        | exception End_of_file -> ()
        | _ -> each_ai_turn state t)
      else (
        print_string ("\n" ^ h.name ^ " is making their turn\n");
        execute_aidecision state h;
        print_string "Enter any key to move to the next players turn\n>";
        match read_line () with
        | exception End_of_file -> ()
        | _ -> each_ai_turn state t)

let next_stage state =
  match state.stage with
  | Pre_Flop -> state.stage <- Flop
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

(* first_k_list k l Returns a list with only the first k elements of l Requires:
   the length of l is greater or equal to k*)
let rec first_k_list k l =
  match k with
  | 0 -> []
  | n -> List.hd l :: first_k_list (k - 1) (List.tl l)

let stage_value state =
  match state.stage with
  | Pre_Flop -> 2
  | Flop -> 3
  | Turn -> 4
  | River -> 5
  | Finish -> 6

let pick_print_winner () =
  if muck_check state then
    print_string ((List.hd (players_in_hand state)).name ^ " wins the round!\n")
  else
    let player_hand_cards = (List.hd state.players).hand in
    let ai_hand_cards = (List.nth state.players 1).hand in
    let ai_hand = Hand.init_hand (ai_hand_cards @ state.deck) in
    let player_hand = Hand.init_hand (player_hand_cards @ state.deck) in
    match Hand.compare ai_hand player_hand with
    | 0 -> print_string "It's a tie! \n"
    | 1 -> print_string "TERA wins the round! \n"
    | -1 -> print_string "Your hand was the best! You win the round! \n"
    | _ -> failwith "not possible"

let rec tick_helper state =
  (if state.stage <> Pre_Flop then
   let v = stage_value state in
   Printer.print_cards (first_k_list v state.community_cards));
  if not (List.hd state.players).folded then print_dealer ();
  if done_betting state then ()
  else (
    if (List.hd state.players).folded then
      print_string
        "\nSince you folded you dont get a turn until the next hand\n"
    else bet_call ();
    print_string "Enter any key to move to the next players turn\n>";
    match read_line () with
    | exception End_of_file -> ()
    | _ ->
        each_ai_turn state (ai_list state.players);
        state.iterated <- true;
        if done_betting state then tick_next state;
        if muck_check state then state.stage <- Finish);
  tick ()

and tick () =
  match state.stage with
  | Pre_Flop ->
      Engine.overturn_community_cards state;
      tick_helper state
  | Flop -> tick_helper state
  | Turn -> tick_helper state
  | River -> tick_helper state
  | Finish -> pick_print_winner ()

let make_player_list player pnum =
  if pnum = 2 then player :: [ Engine.make_player "TERA" 1 ]
  else if pnum = 3 then
    [ player; Engine.make_player "TERA" 1; Engine.make_player "Jerry" 2 ]
  else if pnum = 4 then
    [
      player;
      Engine.make_player "TERA" 1;
      Engine.make_player "Jerry" 2;
      Engine.make_player "Michael Clarkson" 3;
    ]
  else
    [
      player;
      Engine.make_player "TERA" 1;
      Engine.make_player "Jerry" 2;
      Engine.make_player "Michael Clarkson" 3;
      Engine.make_player "Clark Michaelson" 4;
    ]

let rec playgame () =
  Printer.print_intro ();
  match read_line () with
  | exception End_of_file -> ()
  | "yes" -> begin
      Printer.print_rules ();
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
              let plist = make_player_list user pnum in
              state.players <- plist;
              ANSITerminal.print_string [ ANSITerminal.magenta ] "\nWelcome - ";
              print_list playerlst;
              ANSITerminal.print_string [ ANSITerminal.magenta ]
                "\n\nLet's begin the game!\n\n";
              (* print_string (table playername); *)
              print_string "\nEach player has 500 chips.\n";
              print_string deal;
              print_string "\nEnter any key to overturn your cards!\n";
              print_string "> ";
              match read_line () with
              | exception End_of_file -> failwith "error"
              | userchoice ->
                  Engine.deal_cards state;
                  Printer.print_cards (List.hd state.players).hand;
                  print_string "\nRemember your cards :))\n";
                  print_string
                    "\nOther players have also been dealt two cards.\n";

                  tick ()))
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
      | "P" -> playgame ()
      | _ -> ())

let () = playgame ()