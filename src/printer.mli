(** Module to print out frontend of the game.  

    This module reduces code duplication and splits our frontend implementation 
    into two files.*)

open Cards

(** Prints the intoductory ASCII Art and messages.*)
val print_intro : unit -> unit

(** Prints the rules of Poker for the user's reference.*)
val print_rules : unit -> unit

(** Prints the string representation of the cards dealt to the player.*)
val print_deal : unit -> unit

(** Prints the string representation of the cards in the game.*)
val print_cards : card list -> unit
