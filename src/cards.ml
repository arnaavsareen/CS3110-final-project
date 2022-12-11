open Random

type color =
  | Red
  | Black

type suit =
  | Hearts
  | Spades
  | Diamonds
  | Clubs

type number = Number of int

type card = {
  color : color;
  suit : suit;
  number : number;
}

type deck = card list

let suit_arr = [| Hearts; Spades; Diamonds; Clubs |]

let number_arr =
  [|
    Number 1;
    Number 2;
    Number 3;
    Number 4;
    Number 5;
    Number 6;
    Number 7;
    Number 8;
    Number 9;
    Number 10;
    Number 11;
    Number 12;
    Number 13;
    Number 14;
  |]

let rec make_fresh x y deck =
  if x > 3 then deck
  else if y > 13 then make_fresh (x + 1) 0 deck
  else
    make_fresh x (y + 1)
      ({ color = Red; suit = suit_arr.(x); number = number_arr.(y) } :: deck)

let shuffler d =
  let nd = List.map (fun c -> (Random.bits (), c)) d in
  let sond = List.sort compare nd in
  List.map snd sond

let rec draw_helper x deck hand =
  match deck with
  | [] -> (deck, hand)
  | h :: t -> if x = 0 then (deck, hand) else draw_helper (x - 1) t (h :: hand)

let draw x deck = draw_helper x deck []
let fresh_deck = shuffler (make_fresh 0 0 [])

let ascii_suit = function
  | Spades -> "♠"
  | Diamonds -> "♦"
  | Hearts -> "♥"
  | Clubs -> "♣"

let str_number = function
  | Number 14 -> "A"
  | Number 13 -> "Q"
  | Number 12 -> "K"
  | Number 11 -> "J"
  | Number n -> string_of_int n

let shuffle arr =
  Random.self_init ();
  Array.get arr (Random.int (Array.length arr))

let num1 = str_number (shuffle number_arr)
let suit1 = ascii_suit (shuffle suit_arr)
let num2 = str_number (shuffle number_arr)
let suit2 = ascii_suit (shuffle suit_arr)
let num3 = str_number (shuffle number_arr)
let suit3 = ascii_suit (shuffle suit_arr)
let num4 = str_number (shuffle number_arr)
let suit4 = ascii_suit (shuffle suit_arr)
let num5 = str_number (shuffle number_arr)
let suit5 = ascii_suit (shuffle suit_arr)

let deal =
  let card1 = "      ┌─────────┐ ┌─────────┐         \n" in
  let card2 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card3 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card4 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card5 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card6 = "      │░░░░░░░░░│ │░░░░░░░░░│         \n" in
  let card7 = "      └─────────┘ └─────────┘         \n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let overturn_deal =
  let num1 = str_number (shuffle number_arr) in
  let suit1 = ascii_suit (shuffle suit_arr) in
  let num2 = str_number (shuffle number_arr) in
  let suit2 = ascii_suit (shuffle suit_arr) in
  let card1 = "      ┌─────────┐ ┌─────────┐         \n" in
  let card2 =
    "      │ " ^ num1 ^ "       │ │ " ^ num2 ^ "       │         \n"
  in
  let card3 = "      │         │ │         │         \n" in
  let card4 =
    "      │    " ^ suit1 ^ "    │ │    " ^ suit2 ^ "    │         \n"
  in
  let card5 = "      │         │ │         │         \n" in
  let card6 = "      │         │ │         │         \n" in
  let card7 = "      └─────────┘ └─────────┘         \n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let hidden_flop0_str =
  let head1 = " ----------------------------------------------- \n" in
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐      │\n" in
  let card2 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│      │\n" in
  let card3 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│      │\n" in
  let card4 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│      │\n" in
  let card5 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│      │\n" in
  let card6 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│      │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘      │\n" in
  let tail1 = " ----------------------------------------------- \n" in
  head1 ^ card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ tail1

let hidden_flop1_str =
  let head1 = " ----------------------------------------------- \n" in
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐      │\n" in
  let card2 =
    "│      │ " ^ num1 ^ "       │ │░░░░░░░░░│ │░░░░░░░░░│      │\n"
  in
  let card3 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│      │\n" in
  let card4 =
    "│      │    " ^ suit1 ^ "    │ │░░░░░░░░░│ │░░░░░░░░░│      │\n"
  in
  let card5 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│      │\n" in
  let card6 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│      │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘      │\n" in
  let tail1 = " ----------------------------------------------- \n" in
  head1 ^ card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ tail1

let hidden_flop2_str =
  let head1 = " ----------------------------------------------- \n" in
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐      │\n" in
  let card2 =
    "│      │ " ^ num1 ^ "       │ │ " ^ num2 ^ "       │ │░░░░░░░░░│      │\n"
  in
  let card3 = "│      │         │ │         │ │░░░░░░░░░│      │\n" in
  let card4 =
    "│      │    " ^ suit1 ^ "    │ │    " ^ suit2
    ^ "    │ │░░░░░░░░░│      │\n"
  in
  let card5 = "│      │         │ │         │ │░░░░░░░░░│      │\n" in
  let card6 = "│      │         │ │         │ │░░░░░░░░░│      │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘      │\n" in
  let tail1 = " ----------------------------------------------- \n" in
  head1 ^ card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ tail1

let hidden_flop3_str =
  let head1 = " ----------------------------------------------- \n" in
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐      │\n" in
  let card2 =
    "│      │ " ^ num1 ^ "       │ │ " ^ num2 ^ "       │ │ " ^ num3
    ^ "       │      │\n"
  in
  let card3 = "│      │         │ │         │ │         │      │\n" in
  let card4 =
    "│      │    " ^ suit1 ^ "    │ │    " ^ suit2 ^ "    │ │    " ^ suit3
    ^ "    │      │\n"
  in
  let card5 = "│      │         │ │         │ │         │      │\n" in
  let card6 = "│      │         │ │         │ │         │      │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘      │\n" in
  let tail1 = " ----------------------------------------------- \n" in
  head1 ^ card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ tail1

let hidden_flop4_str =
  let head1 = " ----------------------------------------------- \n" in
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐      │\n" in
  let card2 =
    "│      │ " ^ num1 ^ "       │ │ " ^ num2 ^ "       │ │ " ^ num3
    ^ "       │      │\n"
  in
  let card3 = "│      │         │ │         │ │         │      │\n" in
  let card4 =
    "│      │    " ^ suit1 ^ "    │ │    " ^ suit2 ^ "    │ │    " ^ suit3
    ^ "    │      │\n"
  in
  let card5 = "│      │         │ │         │ │         │      │\n" in
  let card6 = "│      │         │ │         │ │         │      │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘      │\n" in
  let card11 = "│      ┌─────────┐                              │\n" in
  let card12 =
    "│      │ " ^ num4 ^ "       │                              │\n"
  in
  let card13 = "│      │         │                              │\n" in
  let card14 =
    "│      │    " ^ suit4 ^ "    │                              │\n"
  in
  let card15 = "│      │         │                              │\n" in
  let card16 = "│      │         │                              │\n" in
  let card17 = "│      └─────────┘                              │\n" in
  let tail1 = " ----------------------------------------------- \n" in
  head1 ^ card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ card11
  ^ card12 ^ card13 ^ card14 ^ card15 ^ card16 ^ card17 ^ tail1

let flop_str =
  let head1 = " ----------------------------------------------- \n" in
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐      │\n" in
  let card2 =
    "│      │ " ^ num1 ^ "       │ │ " ^ num2 ^ "       │ │ " ^ num3
    ^ "       │      │\n"
  in
  let card3 = "│      │         │ │         │ │         │      │\n" in
  let card4 =
    "│      │    " ^ suit1 ^ "    │ │    " ^ suit2 ^ "    │ │    " ^ suit3
    ^ "    │      │\n"
  in
  let card5 = "│      │         │ │         │ │         │      │\n" in
  let card6 = "│      │         │ │         │ │         │      │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘      │\n" in
  let card11 = "│      ┌─────────┐ ┌─────────┐                  │\n" in
  let card12 =
    "│      │ " ^ num4 ^ "       │ │ " ^ num5 ^ "       │                  │\n"
  in
  let card13 = "│      │         │ │         │                  │\n" in
  let card14 =
    "│      │    " ^ suit4 ^ "    │ │    " ^ suit5
    ^ "    │                  │\n"
  in
  let card15 = "│      │         │ │         │                  │\n" in
  let card16 = "│      │         │ │         │                  │\n" in
  let card17 = "│      └─────────┘ └─────────┘                  │\n" in
  let tail1 = " ----------------------------------------------- \n" in
  head1 ^ card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ card11
  ^ card12 ^ card13 ^ card14 ^ card15 ^ card16 ^ card17 ^ tail1

module Hand = struct
  type t =
    (*Many of these types can be represented by a t-uple of cards where t is
      less than 5*)
    | Royal_Flush (*Royal flush always wins, unless it is dealt by the flop*)
    | Straight_Flush of number (*highest card*)
    | Four_of_a_kind of number * number (*four single*)
    | Full_house of number * number (*triple double*)
    | Flush of number * number * number * number * number (*descending value*)
    | Straight of number (*highest card*)
    | Three_of_a_kind of number * number * number (**)
    | Two_pairs of number * number * number
    | Pair of number * number * number * number
    | High_card of number * number * number * number * number

  (*Takes a list of 7 cards, and ordered them first according to multiplicity
    (How often they occur in the list) then by value. Returns an association
    list where every distinct card value is matched to it's multiplicity. For
    example : ordered_card_mult [king, ace, ace, four, four, seven, jack]
    returns [(ace,2);(four,3);(king,1);(jack,1);(seven,1)] Note: in this
    function we don't care about the suits of the cards*)
  let ordered_cards_mult lst =
    let accumulator acc card =
      if List.mem_assoc card.number acc then
        let i = List.assoc card.number acc in
        let templist = List.remove_assoc card.number acc in
        (card.number, i + 1) :: templist
      else (card.number, 1) :: acc
    in
    let templist2 = List.fold_left accumulator [] lst in
    List.rev
      (List.sort_uniq
         (fun (n1, m1) (n2, m2) ->
           match compare m1 m2 with
           | 0 -> compare n1 n2
           | i -> i)
         templist2)

  let number_minus_one n =
    match n with
    | Number i -> Number (i - 1)

  let is_straight (cards : (number * int) list) =
    let count = ref 1 in
    let boolean = ref false in

    (* for i = 0 to List.length cards do if fst (List.nth cards i) =
       number_minus_one (fst (List.nth cards (i-1))) then count := !count + 1;
       if !count = 5 then true else count := 1 ; *)
    let i = ref 0 in
    while !i < List.length cards - 2 && !boolean = false do
      i := !i + 1;
      if
        fst (List.nth cards !i)
        = number_minus_one (fst (List.nth cards (!i + 1)))
      then count := !count + 1;
      if !count = 5 then boolean := true
    done;
    !boolean

  let is_flush (cards : card list) =
    let hearts_counter = ref 0 in
    let spades_counter = ref 0 in
    let clubs_counter = ref 0 in
    let diamonds_counter = ref 0 in
    for i = 0 to List.length cards - 1 do
      match (List.nth cards i).suit with
      | Clubs -> clubs_counter := !clubs_counter + 1
      | Diamonds -> diamonds_counter := !diamonds_counter + 1
      | Hearts -> hearts_counter := !hearts_counter + 1
      | Spades -> spades_counter := !spades_counter + 1
    done;
    if
      !hearts_counter >= 5 || !spades_counter >= 5 || !clubs_counter >= 5
      || !diamonds_counter >= 5
    then true
    else false

  let is_flush2 cards =
    let hearts = List.filter (fun c -> c.suit = Hearts) cards in
    let clubs = List.filter (fun c -> c.suit = Clubs) cards in
    let diamonds = List.filter (fun c -> c.suit = Diamonds) cards in
    let spades = List.filter (fun c -> c.suit = Spades) cards in
    if List.length hearts >= 5 then (true, hearts)
    else if List.length clubs >= 5 then (true, clubs)
    else if List.length diamonds >= 5 then (true, diamonds)
    else if List.length spades >= 5 then (true, spades)
    else (false, spades)

  let init_hand (cards : card list) =
    let is_flush, flush = is_flush2 cards in

    let ordered_cards = ordered_cards_mult cards in
    let is_straight = is_straight ordered_cards in
    (*Straight flush / royal flush case*)
    if is_straight = true && is_flush = true then
      let n = fst (List.hd ordered_cards) in
      if n = Number 14 then Royal_Flush else Straight_Flush n
    else
      (*Four of a kind case*)
      let n1, n2 =
        (fst (List.hd ordered_cards), fst (List.nth ordered_cards 1))
      in
      let m1, m2 =
        (snd (List.hd ordered_cards), snd (List.nth ordered_cards 1))
      in
      match (m1, m2) with
      | 4, _ -> Four_of_a_kind (n1, n2)
      | 3, i when i >= 2 -> Full_house (n1, n2)
      | _ when is_flush ->
          let flush_ordered = ordered_cards_mult flush in
          Flush
            ( fst (List.hd flush_ordered),
              fst (List.nth flush_ordered 1),
              fst (List.nth flush_ordered 2),
              fst (List.nth flush_ordered 3),
              fst (List.nth flush_ordered 4) )
      | 3, _ -> Three_of_a_kind (n1, n2, fst (List.nth ordered_cards 2))
      | 2, 2 -> Two_pairs (n1, n2, fst (List.nth ordered_cards 2))
      | 2, _ ->
          Pair
            ( n1,
              n2,
              fst (List.nth ordered_cards 2),
              fst (List.nth ordered_cards 3) )
      | 1, _ ->
          High_card
            ( n1,
              n2,
              fst (List.nth ordered_cards 2),
              fst (List.nth ordered_cards 3),
              fst (List.nth ordered_cards 4) )
      | _ -> failwith "should not occur"

  (*Compare n tuples is a comparison function between tuples of cards, where a
    tuple is greater than another if the first card value in the tuple is
    greater, if the first values are the same you compare the second values, and
    so on.*)

  let compare t1 t2 =
    let rank t =
      match t with
      | Royal_Flush -> 10
      | Straight_Flush _ -> 9
      | Four_of_a_kind _ -> 8
      | Full_house _ -> 7
      | Flush _ -> 6
      | Straight _ -> 5
      | Three_of_a_kind _ -> 4
      | Two_pairs _ -> 3
      | Pair _ -> 2
      | High_card _ -> 1
    in

    match (t1, t2) with
    | Royal_Flush, Royal_Flush -> 0
    | Straight_Flush c, Straight_Flush b -> compare c b
    | Four_of_a_kind (c1, c2), Four_of_a_kind (b1, b2) ->
        compare (c1, c2) (b1, b2)
    | Full_house (c1, c2), Full_house (b1, b2) -> compare (c1, c2) (b1, b2)
    | Flush (c1, c2, c3, c4, c5), Flush (b1, b2, b3, b4, b5) ->
        compare (c1, c2, c3, c4, c5) (b1, b2, b3, b4, b5)
    | Straight c1, Straight c2 -> compare c1 c2
    | Three_of_a_kind (c1, c2, c3), Three_of_a_kind (b1, b2, b3) ->
        compare (c1, c2, c3) (b1, b2, b3)
    | Two_pairs (c1, c2, c3), Two_pairs (b1, b2, b3) ->
        compare (c1, c2, c3) (b1, b2, b3)
    | Pair (c1, c2, c3, c4), Pair (b1, b2, b3, b4) ->
        compare (c1, c2, c3, c4) (b1, b2, b3, b4)
    | High_card (c1, c2, c3, c4, c5), High_card (b1, b2, b3, b4, b5) ->
        compare (c1, c2, c3, c4, c5) (b1, b2, b3, b4, b5)
    | _ -> compare (rank t1) (rank t2)

  let to_string t =
    match t with
    | Royal_Flush -> "Royal Flush"
    | Straight_Flush (Number n) -> "Straight Flush " ^ string_of_int n
    | Four_of_a_kind (Number n, Number n2) ->
        "Four_of_a_kind " ^ string_of_int n ^ ", " ^ string_of_int n2
    | Full_house (Number n, Number n2) ->
        "Full_House " ^ string_of_int n ^ ", " ^ string_of_int n2
    | Flush (Number n1, Number n2, Number n3, Number n4, Number n5) ->
        "Flush " ^ string_of_int n1 ^ ", " ^ string_of_int n2 ^ ", "
        ^ string_of_int n3 ^ ", " ^ string_of_int n4 ^ ", " ^ string_of_int n5
    | Straight (Number n1) -> "Straight " ^ string_of_int n1
    | Two_pairs (Number n1, Number n2, Number n3) ->
        "Two_pairs " ^ string_of_int n1 ^ ", " ^ string_of_int n2 ^ ", "
        ^ string_of_int n3
    | Pair (Number n1, Number n2, Number n3, Number n4) ->
        "Pair " ^ string_of_int n1 ^ ", " ^ string_of_int n2 ^ ", "
        ^ string_of_int n3 ^ ", " ^ string_of_int n4
    | High_card (Number n1, Number n2, Number n3, Number n4, Number n5) ->
        "High_card " ^ string_of_int n1 ^ ", " ^ string_of_int n2 ^ ", "
        ^ string_of_int n3 ^ ", " ^ string_of_int n4 ^ ", " ^ string_of_int n5
        ^ ", "
    | Three_of_a_kind (Number n1, Number n2, Number n3) ->
        "Flush " ^ string_of_int n1 ^ ", " ^ string_of_int n2 ^ ", "
        ^ string_of_int n3
end

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