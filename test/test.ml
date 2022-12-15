open OUnit2
open Game
open Cards
open Engine

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
let aD = { color = Red; suit = Diamonds; number = Number 14 }
let aS = { color = Black; suit = Spades; number = Number 14 }
let kC = { color = Black; suit = Clubs; number = Number 13 }
let kD = { color = Red; suit = Diamonds; number = Number 13 }
let kH = { color = Red; suit = Hearts; number = Number 13 }
let kS = { color = Black; suit = Spades; number = Number 13 }
let jC = { color = Black; suit = Clubs; number = Number 11 }
let fourC = { color = Black; suit = Clubs; number = Number 4 }
let fourH = { color = Red; suit = Hearts; number = Number 4 }
let sixD = { color = Red; suit = Diamonds; number = Number 6 }
let sixH = { color = Red; suit = Hearts; number = Number 6 }
let sixC = { color = Black; suit = Clubs; number = Number 6 }
let sevenH = { color = Red; suit = Hearts; number = Number 7 }
let eightC = { color = Black; suit = Clubs; number = Number 8 }
let aS = { color = Black; suit = Spades; number = Number 14 }
let qC = { color = Black; suit = Clubs; number = Number 12 }
let qH = { color = Red; suit = Hearts; number = Number 12 }
let jC = { color = Black; suit = Clubs; number = Number 11 }
let tenC = { color = Black; suit = Clubs; number = Number 10 }
let nineC = { color = Black; suit = Clubs; number = Number 9 }
let nineH = { color = Red; suit = Hearts; number = Number 9 }

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

let hand_module_tests =
  [
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
    (*ordered_card_mult_test*)
    ordered_card_mult_test "four of a kind"
      [ aC; aH; kC; aC; aH; sevenH; fourH ]
      [ (Number 14, 4); (Number 13, 1); (Number 7, 1); (Number 4, 1) ];
    ordered_card_mult_test "Royal Flush"
      [ aC; fourH; jC; fourC; kC; qC; tenC ]
      [
        (Number 4, 2);
        (Number 14, 1);
        (Number 13, 1);
        (Number 12, 1);
        (Number 11, 1);
        (Number 10, 1);
      ];
    (*card to string test*)
    ( "test card to string" >:: fun _ ->
      assert_equal "7 of Hearts" (card_to_string sevenH) );
    ( "test card list to string" >:: fun _ ->
      assert_equal "[7 of Hearts; 13 of Clubs; ]"
        (list_to_string card_to_string [ sevenH; kC ])
        ~printer:(fun x -> x) );
    (*init_hand tests*)
    init_hand_test "Royal Flush"
      [ aC; fourH; jC; fourC; kC; qC; tenC ]
      "Royal Flush";
    init_hand_test "four of a kind"
      [ aC; aH; kC; kS; aC; aH; sevenH; fourH ]
      "Four of a kind 14, 13";
    init_hand_test "four of a kind 2"
      [ kC; aH; kC; kS; aC; kH; sevenH; fourH ]
      "Four of a kind 13, 14";
    init_hand_test "Straight Flush"
      [ nineC; fourH; jC; kS; kC; qC; tenC ]
      "Straight Flush 13";
    init_hand_test "Full house 13 14"
      [ aC; aH; jC; kS; kC; jC; kC ]
      "Full House 13, 14";
    init_hand_test "Flush that has straight for different cards"
      [ aC; tenC; jC; kS; kC; fourC; qH ]
      "Flush 14, 13, 11, 10, 4";
    init_hand_test "Straight"
      [ aC; tenC; jC; eightC; sixD; nineH; qH ]
      "Straight 12";
    init_hand_test "Other Straight"
      [ aC; tenC; sevenH; eightC; sixD; nineH; kH ]
      "Straight 10";
    init_hand_test "Straight and three of a kind is straight"
      [ sixH; tenC; sevenH; eightC; sixD; nineH; sixC ]
      "Straight 10";
    init_hand_test "Three of a kind"
      [ sixH; tenC; sevenH; kC; sixD; nineH; sixC ]
      "Three of a kind 6, 13, 10";
    init_hand_test "Two pairs"
      [ kH; tenC; sevenH; kC; sixD; nineH; sixC ]
      "Two pairs 13, 6, 10";
    init_hand_test "High card"
      [ kH; tenC; sevenH; fourC; jC; nineH; sixC ]
      "High card 13, 11, 10, 9, 7";
    init_hand_test "Full house with 5 cards to check if it works"
      [ aC; aH; kS; kC; kC ] "Full House 13, 14";
  ]

let tests =
  "test suite for cards.ml"
  >::: hand_module_tests
       @ [
           ("random" >:: fun _ -> assert_equal 1 1);
           (* Testing for betting*)
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
         ]

let _ = run_test_tt_main tests
