open OUnit2
open Deck
open Command
open State
open Player
(* open Game *)


(** [make_start_room_test name adv expected_output] 
    constructs an OUnit test named [name] that checks whether 
    [start_room] returns the same value
    as [expected_output] *)
let make_generate_cards_of_suit_test
    (name : string) 
    (st: Deck.suit) 
    (loop: int list)
    (acc : Deck.t)
    (expected_output : Deck.card list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (generate_cards_of_suit st loop acc))

let cards_of_suit_tests =
  [
    make_generate_cards_of_suit_test 
      "One Card"  Heart [1] [] [{suit = Heart; value = 1}];
    make_generate_cards_of_suit_test 
      "Two Cards"  Heart [1;2] [] [{suit = Heart; value = 2};
                                   {suit = Heart; value = 1}];
    make_generate_cards_of_suit_test 
      "Not starting at 1"  Heart [4; 7; 8] [] [{suit = Heart; value = 8}; 
                                               {suit = Heart; value = 7};
                                               {suit = Heart; value = 4}];
    make_generate_cards_of_suit_test 
      "Spade"  Spade [1] [] [{suit = Spade; value = 1}];
    make_generate_cards_of_suit_test 
      "Club"  Club [1] [] [{suit = Club; value = 1}];
    make_generate_cards_of_suit_test 
      "Heart"  Heart [1] [] [{suit = Heart; value = 1}];
  ]

let make_new_deck_test
    (name : string) 
    (expected_output : Deck.card list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.new_deck))


let standard_deck = [{suit = Heart; value = 13}; {suit = Heart; value = 12};
                     {suit = Heart; value = 11}; {suit = Heart; value = 10};
                     {suit = Heart; value = 9}; {suit = Heart; value = 8};
                     {suit = Heart; value = 7}; {suit = Heart; value = 6};
                     {suit = Heart; value = 5}; {suit = Heart; value = 4};
                     {suit = Heart; value = 3}; {suit = Heart; value = 2};
                     {suit = Heart; value = 1}; {suit = Diamond; value = 13};
                     {suit = Diamond; value = 12}; {suit = Diamond; value = 11};
                     {suit = Diamond; value = 10}; {suit = Diamond; value = 9};
                     {suit = Diamond; value = 8}; {suit = Diamond; value = 7};
                     {suit = Diamond; value = 6}; {suit = Diamond; value = 5};
                     {suit = Diamond; value = 4}; {suit = Diamond; value = 3};
                     {suit = Diamond; value = 2}; {suit = Diamond; value = 1};
                     {suit = Spade; value = 13}; {suit = Spade; value = 12};
                     {suit = Spade; value = 11}; {suit = Spade; value = 10};
                     {suit = Spade; value = 9}; {suit = Spade; value = 8};
                     {suit = Spade; value = 7}; {suit = Spade; value = 6};
                     {suit = Spade; value = 5}; {suit = Spade; value = 4};
                     {suit = Spade; value = 3}; {suit = Spade; value = 2};
                     {suit = Spade; value = 1}; {suit = Club; value = 13};
                     {suit = Club; value = 12}; {suit = Club; value = 11};
                     {suit = Club; value = 10}; {suit = Club; value = 9};
                     {suit = Club; value = 8};
                     {suit = Club; value = 7}; {suit = Club; value = 6}; 
                     {suit = Club; value = 5};
                     {suit = Club; value = 4}; {suit = Club; value = 3}; 
                     {suit = Club; value = 2};
                     {suit = Club; value = 1}]

let new_deck_test = [
  make_new_deck_test "New Deck" standard_deck;
]

let make_choose_test
    (name : string) 
    (deck: Deck.t)
    (expected_output : (card * Deck.t) option) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.choose standard_deck))

let choose_tests = [Some
                      ({suit = Heart; value = 13},
                       [{suit = Heart; value = 12}; {suit = Heart; value = 11};
                        {suit = Heart; value = 10}; {suit = Heart; value = 9};
                        {suit = Heart; value = 8}; {suit = Heart; value = 7};
                        {suit = Heart; value = 6}; {suit = Heart; value = 5};
                        {suit = Heart; value = 4}; {suit = Heart; value = 3};
                        {suit = Heart; value = 2}; {suit = Heart; value = 1};
                        {suit = Diamond; value = 13};
                        {suit = Diamond; value = 12};
                        {suit = Diamond; value = 11};
                        {suit = Diamond; value = 10};
                        {suit = Diamond; value = 9};{suit = Diamond; value = 8};
                        {suit = Diamond; value = 7};{suit = Diamond; value = 6};
                        {suit = Diamond; value = 5};{suit = Diamond; value = 4};
                        {suit = Diamond; value = 3};{suit = Diamond; value = 2};
                        {suit = Diamond; value = 1}; {suit = Spade; value = 13};
                        {suit = Spade; value = 12}; {suit = Spade; value = 11};
                        {suit = Spade; value = 10}; {suit = Spade; value = 9};
                        {suit = Spade; value = 8}; {suit = Spade; value = 7};
                        {suit = Spade; value = 6}; {suit = Spade; value = 5};
                        {suit = Spade; value = 4}; {suit = Spade; value = 3};
                        {suit = Spade; value = 2}; {suit = Spade; value = 1};
                        {suit = Club; value = 13}; {suit = Club; value = 12};
                        {suit = Club; value = 11}; {suit = Club; value = 10};
                        {suit = Club; value = 9}; {suit = Club; value = 8};
                        {suit = Club; value = 7}; {suit = Club; value = 6};
                        {suit = Club; value = 5}; {suit = Club; value = 4};
                        {suit = Club; value = 3}; {suit = Club; value = 2};
                        {suit = Club; value = 1}]);

                   ]


let make_val_of_test
    (name : string) 
    (card: Deck.card)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.val_of card))

let val_of_tests = [
  make_val_of_test
    "Ace" {suit = Heart; value = 1} 11;
  make_val_of_test
    "Ten" {suit = Heart; value = 10} 10;
  make_val_of_test
    "Assert Jack value is 10" {suit = Heart; value = 11} 10;
  make_val_of_test
    "Assert Queen value is 10" {suit = Heart; value = 12} 10;
  make_val_of_test
    "Assert King Value is 10" {suit = Heart; value = 13} 10;

]

let make_length_test
    (name : string) 
    (deck: Deck.t)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.length deck))

let length_tests =
  [
    make_length_test "standard deck = 52" standard_deck 52;
    make_length_test "length one deck" [{suit = Heart; value = 1}] 1;
  ]

let make_command_test
    (name : string) 
    (str: string) 
    (expected_output : command) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (parse str))

let command_tests =
  [
    make_command_test "Testing Hit command" "Hit" Hit;
    make_command_test "Testing h command" "h" Hit;
    make_command_test "Testing hit command" "hit" Hit;
    make_command_test "Testing Doulbe Down command" "Double Down" Double;
    make_command_test "Testing double down Command" "double down" Double;
    make_command_test "Testing Double" "Double" Double;
    make_command_test "Testing Down Command" "Down" Double;
    make_command_test "Testing double Command" "double" Double;
    make_command_test "Testing down Command" "down" Double;
    make_command_test "Testing d Command" "d" Double;
    "Malformed"    >:: (fun _ -> assert_raises (Malformed) 
                           (fun () -> parse "Hello"));
  ]
let make_new_player_test
    (name : string) 
    (id : player_id) 
    (starting_cash : player_money)
    (expected_output : Player.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (new_player id starting_cash))

let player_1 = 
  {id = 1; hand = []; count = 0; cash = 200}
let player_2 = 
  {id = 1; hand = []; count = 0; cash = 500}

let new_player_test = 
  [
    make_new_player_test "Player 1 200 dollars" 1 200 player_1;
    make_new_player_test "Player 2 500 dollars" 1 500 player_2;
  ]

let suite =
  "test suite for A2" >::: List.flatten [
    cards_of_suit_tests;
    new_deck_test;
    val_of_tests;
    length_tests;
    command_tests;
    new_player_test;
  ]

let _ = run_test_tt_main suite


