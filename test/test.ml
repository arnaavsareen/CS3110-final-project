open OUnit2
open Game
open Cards

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
  name >:: fun _ -> assert_equal expected_output (Hand.init_hand cards)

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
             ~printer:(fun x -> x) )
         (* (init_hand_test "four of a kind" [ aC; aH; kC; aC; aH; sevenH; fourH
            ] (Four_of_a_kind (Number 14, Number 13)) *);
       ]

let _ = run_test_tt_main tests
