let rec playgame () =
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\
    \     /$$$$$$   /$$$$$$   /$$$$$$  /$$$$$$ /$$  /$$   /$$$$$$\n\
    \    /$$__  $$ /$$__  $$ /$$__ $$ |_ $$_/ | $$$ | $$ /$$    $$\n\
    \   | $$    _/| $$   $$| $$    _/   | $$  | $$$$| $$| $$    $$\n\
    \   | $$      | $$$$$$$$|  $$$$$$   | $$  | $$ $$ $$| $$    $$\n\
    \   | $$      | $$__  $$|  ___  $$  | $$  | $$  $$$$| $$    $$\n\
    \   | $$    $$| $$  | $$ / $$   $$  | $$  | $$  $$$ | $$    $$\n\
    \   |  $$$$$$/| $$  | $$|  $$$$$$/ /$$$$$$| $$   $$ |  $$$$$$/\n\
    \   |______/  |__/  |__/   _____/ |______/|__/   |__/  ______/\n\
    \                                                          \n\
    \                                                        \n\
    \  ";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    " \n\
    \       /$$$$$$  /$$$$$$        /$$$$$$$                         \n\
    \      /$$__  $$/$$__  $$      | $$ __ $$                        \n\
    \     | $$    _/ $$    _/      | $$    $$ / $$  / $$ /$$$$$$/$$$$\n\
    \     | $$     |  $$$$$$       | $$$$$$$ | $$ | $$|  $$ _ $$ _ $$\n\
    \     | $$        ___  $$      | $$ __ $$| $$ | $$|  $$   $$   $$\n\
    \     | $$    $$/$$   $$       | $$   $$ | $$ | $$|  $$ | $$ | $$\n\
    \     |  $$$$$$/  $$$$$$/      | $$$$$$$/| $$$$$$ /| $$ | $$ | $$\n\
    \     |_______/   _____/       |_______/    _____/ |__/ |__/ |__/\n\
    \                                                              \n\
    \                                                              \n\
    \                                                              ";
  ANSITerminal.print_string [ ANSITerminal.white ]
    "\n\
     CS 3110 Final Project by Arnaav Sareen, Tyler Forstrom, Eric Yang, and \
     Ryan Noonan\n";
  ANSITerminal.print_string [ ANSITerminal.magenta ]
    "\n\nAre you ready to play some poker today? (yes/no)\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | userchoice ->
      if userchoice = "yes" then
        ANSITerminal.(
          print_string [ red ]
            "Let's start by reviewing the rules of our game!\n\
            \ \n\
             We are going to be playing Texas Hold'em Poker (the most popular \
             of all poker variations)\n\
            \ > In a game of Texas hold'em, each player is dealt two cards \
             face down (the 'hole cards')\n\
            \ > Over several betting rounds, five more cards are (eventually) \
             dealt face up in the middle of the table\n\
            \ > These face-up cards are called the 'community cards.' Each \
             player is free to use the community cards in combination with \
             their hole cards to build a five-card poker hand.\n\
            \ > Put simply, the person with th best hand wins. But there is a \
             lot of strategy involved to be good at poker. A well known \
             technique is 'bluffing'.\n\
             credits: https://www.pokernews.com/poker-rules/texas-holdem.htm")
      else if userchoice = "no" then
        ANSITerminal.(
          print_string [ red ] "Quiting the game. See you next time :))")
      else
        ANSITerminal.(
          print_string [ red ] "Wrong input. Try again!";
          playgame ())

let () = playgame ()
