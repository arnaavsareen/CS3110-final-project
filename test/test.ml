(************************************************************
   Our Approach to Testing - 

   We feel that the best way to test our game of poker is to play it. Being a 
   user is one of the best ways to find bugs in our program, which is why we 
   used the maunal testing methodology extensively. 
   Using both the glass box and black box testing methods, we made sure to test 
   the Cards, Engine and Ai modules that we developed. 
   We created sample cards and players to test the functions written in our 
   modules which were capable of non unit() outputs.  
   Functions which had their outputs as unit() were tested maunually by running
   our game. 
 ************************************************************)
open OUnit2
open Game
open Cards
open Engine
open Ai

let card_to_string c =
  match c with
  | { suit = Clubs; number = Number i } -> string_of_int i ^ " of Clubs"
  | { suit = Hearts; number = Number i } -> string_of_int i ^ " of Hearts"
  | { suit = Spades; number = Number i } -> string_of_int i ^ " of Spades"
  | { suit = Diamonds; number = Number i } -> string_of_int i ^ " of Diamonds"

let rec list_to_string f lst =
  let rec matching lst =
    match lst with
    | [] -> "]"
    | h :: t -> f h ^ "; " ^ matching t
  in
  "[" ^ matching lst

let int_card_pair_to_string (c, i) =
  "(" ^ card_to_string c ^ ", " ^ string_of_int i ^ ")"

let number_int_pair_to_string (n, i) =
  "("
  ^ string_of_int
      (match n with
      | Number a -> a)
  ^ ", " ^ string_of_int i ^ ")"

(*Creating cards for testing*)
let aC = { color = Black; suit = Clubs; number = Number 14 }
let aH = { color = Red; suit = Hearts; number = Number 14 }
let kC = { color = Black; suit = Clubs; number = Number 13 }
let jC = { color = Black; suit = Clubs; number = Number 11 }
let fourC = { color = Black; suit = Clubs; number = Number 4 }
let fourH = { color = Red; suit = Hearts; number = Number 4 }
let sevenH = { color = Red; suit = Hearts; number = Number 7 }

let ordered_card_mult_test name card_list expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Hand.ordered_cards_mult card_list)
    ~printer:(list_to_string number_int_pair_to_string)

let init_hand_test name cards expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (Hand.to_string (Hand.init_hand cards))
    ~printer:(fun x -> x)

let call_amount_test name player turn_order expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (call_amount player turn_order)
    ~printer:string_of_int

let make_bet_test name player turn_order int expected_output =
  name >:: fun _ ->
  assert_equal expected_output (make_bet player turn_order int)
    ~printer:(fun x -> string_of_int x.bet)

let top_bet_test name turn_order expected_output =
  name >:: fun _ ->
  assert_equal expected_output (top_bet turn_order) ~printer:string_of_int

let is_side_pot_test name turn_order expected_output =
  name >:: fun _ ->
  assert_equal expected_output (is_side_pot turn_order) ~printer:string_of_bool

let bet_list_test name turn_order expected_output =
  name >:: fun _ ->
  assert_equal expected_output (bet_list turn_order) ~printer:(fun a ->
      list_to_string string_of_int a)

let fix_value_test name list_int expected_output =
  name >:: fun _ ->
  assert_equal expected_output (fix_values list_int) ~printer:(fun a ->
      list_to_string string_of_int a)

let pot_amounts_test name turn_order list_int expected_output =
  name >:: fun _ ->
  assert_equal expected_output (pot_amounts turn_order list_int)
    ~printer:(fun a -> list_to_string string_of_int a)

let done_betting_help_test name turn_order expected_output =
  name >:: fun _ ->
  assert_equal expected_output
    (done_betting_help turn_order)
    ~printer:string_of_bool

let done_betting_test name status expected_output =
  name >:: fun _ ->
  assert_equal expected_output (done_betting status) ~printer:string_of_bool

let ascii_suit_test name input expected_output =
  name >:: fun _ -> assert_equal expected_output (ascii_suit input)

let str_number_test name input expected_output =
  name >:: fun _ -> assert_equal expected_output (str_number input)

let ai_check = function
  | Fold -> true
  | Check -> true
  | Raise -> true
  | Call -> true

let ai_bet_test name bet pool plyr stage =
  name >:: fun _ ->
  assert_equal true
    (ai_check (make_decision bet pool plyr stage))
    ~printer:string_of_bool

let tyler =
  {
    name = "tyler";
    hand = [ fourC; sevenH ];
    money = 1000;
    bet = 200;
    folded = false;
    position = 0;
  }

let arnaav =
  {
    name = "arnaav";
    hand = [ aH; aC ];
    money = 0;
    bet = 90;
    folded = false;
    position = 1;
  }

let ryan =
  {
    name = "ryan";
    hand = [ sevenH; sevenH ];
    money = 690;
    bet = 420;
    folded = false;
    position = 2;
  }

let eric =
  {
    name = "eric";
    hand = [ kC; fourH ];
    money = 100;
    bet = 200;
    folded = false;
    position = 3;
  }

let big_man =
  {
    name = "big man";
    hand = [ kC; fourH ];
    money = 0;
    bet = 100;
    folded = false;
    position = 4;
  }

let fold_dude =
  {
    name = "fold dude";
    hand = [ kC; fourH ];
    money = 0;
    bet = 200;
    folded = true;
    position = 5;
  }

let plist = [ tyler; arnaav; ryan; eric ]
let plist1 = [ tyler; big_man; arnaav; ryan; eric ]
let plist2 = [ tyler; big_man; arnaav; fold_dude ]
let plist3 = [ tyler; big_man; arnaav; eric ]
let plist4 = [ tyler; fold_dude; arnaav; eric ]
let test_list = [ 90; 100 ]
let state = Engine.init_state

let state2 =
  {
    stage = Pre_Flop;
    players = [];
    pot = { amount = 0; side_pots = [] };
    deck = init_shuffled_deck;
    community_cards = [];
    current_bet = 10;
    options = { starting_money = 0 };
    iterated = true;
    rounds = 4;
  }

let state3 =
  {
    stage = Pre_Flop;
    players = [ ryan; eric ];
    pot = { amount = 0; side_pots = [] };
    deck = init_shuffled_deck;
    community_cards = [];
    current_bet = 10;
    options = { starting_money = 0 };
    iterated = true;
    rounds = 4;
  }

let state4 =
  {
    stage = Pre_Flop;
    players = [ ryan; eric ];
    pot = { amount = 0; side_pots = [] };
    deck = init_shuffled_deck;
    community_cards = [];
    current_bet = 10;
    options = { starting_money = 0 };
    iterated = true;
    rounds = 4;
  }

let ai_tests =
  [
    ai_bet_test "Testing the AI1" 100
      [
        { color = Red; suit = Hearts; number = Number 8 };
        { color = Black; suit = Clubs; number = Number 9 };
      ]
      ryan Pre_Flop;
    ai_bet_test "Testing the AI2" 0
      [
        { color = Red; suit = Hearts; number = Number 8 };
        { color = Black; suit = Clubs; number = Number 9 };
      ]
      eric Pre_Flop;
    ai_bet_test "Testing the AI3" 50
      [
        { color = Red; suit = Hearts; number = Number 8 };
        { color = Black; suit = Clubs; number = Number 9 };
      ]
      arnaav Pre_Flop;
    ai_bet_test "Testing the AI4" 75
      [
        { color = Red; suit = Hearts; number = Number 8 };
        { color = Black; suit = Clubs; number = Number 9 };
      ]
      big_man Pre_Flop;
  ]

let tests =
  "test suite for cards.ml"
  >::: [
         ("random" >:: fun _ -> assert_equal 1 1);
         (*Tests for Hand module*)
         ordered_card_mult_test "random 7 cards"
           [ aC; aH; kC; jC; fourC; fourH; sevenH ]
           [
             (Number 14, 2);
             (Number 4, 2);
             (Number 13, 1);
             (Number 11, 1);
             (Number 7, 1);
           ];
         ordered_card_mult_test "four of a kind"
           [ aC; aH; kC; aC; aH; sevenH; fourH ]
           [ (Number 14, 4); (Number 13, 1); (Number 7, 1); (Number 4, 1) ];
         ( "test card to string" >:: fun _ ->
           assert_equal "7 of Hearts" (card_to_string sevenH) );
         ( "test card list to string" >:: fun _ ->
           assert_equal "[7 of Hearts; 13 of Clubs; ]"
             (list_to_string card_to_string [ sevenH; kC ])
             ~printer:(fun x -> x) );
         init_hand_test "four of a kind"
           [ aC; aH; kC; aC; aH; sevenH; fourH ]
           "Four_of_a_kind 14, 13";
         top_bet_test "testing top bet" plist 420;
         call_amount_test "testing all in" eric plist 100;
         call_amount_test "testing regular call" tyler plist 220;
         make_bet_test "testing bet over money" tyler plist 1100 tyler;
         make_bet_test "testing bet below top bet" tyler plist 100 tyler;
         make_bet_test "testing valid bet" tyler plist 220
           {
             name = "tyler";
             hand = [ fourC; sevenH ];
             money = 780;
             bet = 420;
             folded = false;
             position = 0;
           };
         done_betting_help_test "testing if done" plist2 true;
         done_betting_test "Done_betting-1" state false;
         done_betting_test "Done_betting-2" state2 true;
         done_betting_test "Done_betting-3" state3 true;
         is_side_pot_test "testing side pot amount" plist true;
         fix_value_test "testing fix values" test_list [ 90; 10 ];
         bet_list_test "testing side pot" plist [ 90; 200 ];
         bet_list_test "testing side pot" plist1 [ 90; 100; 200 ];
         bet_list_test "testing sidepot" plist3 [ 90; 100; 200 ];
         pot_amounts_test "testing pot amount" plist3 [ 90; 10; 100 ]
           [ 360; 30; 200 ];
         pot_amounts_test "testing combination" plist3
           (fix_values (bet_list plist3))
           [ 360; 30; 200 ];
         ascii_suit_test "Testing ascii_suit Spades" Spades "♠";
         ascii_suit_test "Testing ascii_suit Hearts" Hearts "♥";
         ascii_suit_test "Testing ascii_suit Diamonds" Diamonds "♦";
         ascii_suit_test "Testing ascii_suit Clubs" Clubs "♣";
         str_number_test "Testing string representation of numbers" (Number 14)
           "A";
         str_number_test "Testing string representation of numbers" (Number 13)
           "Q";
         str_number_test "Testing string representation of numbers" (Number 12)
           "K";
         str_number_test "Testing string representation of numbers" (Number 11)
           "J";
         str_number_test "Testing string representation of numbers" (Number 10)
           "A";
         str_number_test "Testing string representation of numbers" (Number 9)
           "9";
         str_number_test "Testing string representation of numbers" (Number 8)
           "8";
         str_number_test "Testing string representation of numbers" (Number 7)
           "7";
         str_number_test "Testing string representation of numbers" (Number 6)
           "6";
         str_number_test "Testing string representation of numbers" (Number 5)
           "5";
         str_number_test "Testing string representation of numbers" (Number 4)
           "4";
         str_number_test "Testing string representation of numbers" (Number 3)
           "3";
         str_number_test "Testing string representation of numbers" (Number 2)
           "2";
       ]
       @ ai_tests

let _ = run_test_tt_main tests
