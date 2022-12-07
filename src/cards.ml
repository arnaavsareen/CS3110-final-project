open Random

type color =
  | Red
  | Black

(* Hi I am here.*)
(*another thing is here*)
(*A third thing has appeared!*)
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
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐        │\n" in
  let card2 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card3 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card4 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card5 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card6 = "│      │░░░░░░░░░│ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘        │\n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let hidden_flop1_str =
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐        │\n" in
  let card2 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card3 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card4 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card5 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card6 = "│      │         │ │░░░░░░░░░│ │░░░░░░░░░│        │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘        │\n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let hidden_flop2_str =
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐        │\n" in
  let card2 = "│      │         │ │         │ │░░░░░░░░░│        │\n" in
  let card3 = "│      │         │ │         │ │░░░░░░░░░│        │\n" in
  let card4 = "│      │         │ │         │ │░░░░░░░░░│        │\n" in
  let card5 = "│      │         │ │         │ │░░░░░░░░░│        │\n" in
  let card6 = "│      │         │ │         │ │░░░░░░░░░│        │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘        │\n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let hidden_flop3_str =
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐        │\n" in
  let card2 = "│      │         │ │         │ │         │        │\n" in
  let card3 = "│      │         │ │         │ │         │        │\n" in
  let card4 = "│      │         │ │         │ │         │        │\n" in
  let card5 = "│      │         │ │         │ │         │        │\n" in
  let card6 = "│      │         │ │         │ │         │        │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘        │\n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7

let flop_str =
  let card1 = "│      ┌─────────┐ ┌─────────┐ ┌─────────┐        │\n" in
  let card2 = "│      │         │ │         │ │         │        │\n" in
  let card3 = "│      │         │ │         │ │         │        │\n" in
  let card4 = "│      │         │ │         │ │         │        │\n" in
  let card5 = "│      │         │ │         │ │         │        │\n" in
  let card6 = "│      │         │ │         │ │         │        │\n" in
  let card7 = "│      └─────────┘ └─────────┘ └─────────┘        │\n" in
  let card8 = "│      ┌─────────┐ ┌─────────┐                    │\n" in
  let card9 = "│      │         │ │         │                    │\n" in
  let card10 = "│     │         │ │         │                    │\n" in
  let card11 = "│     │         │ │         │                    │\n" in
  let card12 = "│     │         │ │         │                    │\n" in
  let card13 = "│     │         │ │         │                    │\n" in
  let card14 = "│     └─────────┘ └─────────┘                    │\n" in
  card1 ^ card2 ^ card3 ^ card4 ^ card5 ^ card6 ^ card7 ^ card8 ^ card9 ^ card10
  ^ card11 ^ card12 ^ card13 ^ card14

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
    let i = 0 in
    while i < List.length cards - 2 && !boolean = false do
      if
        fst (List.nth cards i) = number_minus_one (fst (List.nth cards (i + 1)))
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

  let init_hand (cards : card list) =
    let is_flush = is_flush cards in
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
    match (t1, t2) with
    | Royal_Flush, Royal_Flush -> 0
    | Straight_Flush c, Straight_Flush b -> 0
    | Four_of_a_kind (c1, c2), Four_of_a_kind (b1, b2) ->
        Stdlib.compare (c1, c2) (b1, b2)
    | Full_house (c1, c2), Full_house (b1, b2) -> 0
    | Flush (c1, c2, c3, c4, c5), Flush (b1, b2, b3, b4, b5) -> 0
    | Straight c1, Straight c2 -> 0
    | Three_of_a_kind (c1, c2, c3), Three_of_a_kind (b1, b2, b3) -> 0
    | Two_pairs (c1, c2, c3), Two_pairs (b1, b2, b3) -> 0
    | Pair (c1, c2, c3, c4), Pair (b1, b2, b3, b4) -> 0
    | High_card (c1, c2, c3, c4, c5), High_card (b1, b2, b3, b4, b5) -> 0
    | _, _ -> 0
end
