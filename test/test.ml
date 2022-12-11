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
    ~printer:(fun x -> x.name)

let top_bet_test name turn_order expected_output =
  name >:: fun _ ->
  assert_equal expected_output (top_bet turn_order) ~printer:string_of_int

let is_side_pot_test name turn_order expected_output =
  name >:: fun _ ->
  assert_equal expected_output (is_side_pot turn_order) ~printer:string_of_bool

let side_pot_list_test name turn_order expected_output =
  name >:: fun _ ->
  assert_equal expected_output (side_pot_list turn_order) ~printer:(fun a ->
      list_to_string string_of_int a)

let fix_value_test name list_int expected_output =
  name >:: fun _ ->
  assert_equal expected_output (fix_values list_int) ~printer:(fun a ->
      list_to_string string_of_int a)

let total_side_pot_player_test name turn_order int expected_output =
  name >:: fun _ ->
  assert_equal expected_output (total_side_pot_player turn_order int)
    ~printer:(fun a -> list_to_string string_of_int a)

let tyler =
  {
    name = "tyler";
    hand = [ fourC; sevenH ];
    money = 1000;
    bet = 100;
    folded = false;
  }

let arnaav =
  { name = "arnaav"; hand = [ aH; aC ]; money = 0; bet = 90; folded = false }

let ryan =
  {
    name = "ryan";
    hand = [ sevenH; sevenH ];
    money = 690;
    bet = 420;
    folded = false;
  }

let eric =
  {
    name = "eric";
    hand = [ kC; fourH ];
    money = 100;
    bet = 200;
    folded = false;
  }

let big_man =
  {
    name = "big man";
    hand = [ kC; fourH ];
    money = 0;
    bet = 100;
    folded = false;
  }

let plist = [ tyler; arnaav; ryan; eric ]
let plist1 = [ tyler; big_man; arnaav; ryan; eric ]
let test_list = [ 90; 100 ]

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
         call_amount_test "testing regular call" tyler plist 320;
         make_bet_test "testing bet over money" tyler plist 1100 tyler;
         make_bet_test "testing bet below top bet" tyler plist 100 tyler;
         make_bet_test "testing valid bet" tyler plist 320
           {
             name = "tyler";
             hand = [ fourC; sevenH ];
             money = 680;
             bet = 420;
             folded = false;
           };
         is_side_pot_test "testing side pot  amount" plist true;
         fix_value_test "testing fix values" test_list [ 90; 10 ];
         side_pot_list_test "testing side pot" plist [ 90 ];
         side_pot_list_test "testing side pot" plist1 [ 90; 10 ];
       ]

let _ = run_test_tt_main tests
