open OUnit2
open Deck
open Command
open State
open Player
(* open Game *)


(** Test Plan: For our game of Blackjack, we wrote Ounit tests for the Deck,
    Command, and Player modules. We wrote Ounit tests for these three modules
    since these arrise the most possibility to be incorrect without us noticing
    while playing the game. For example we might not realize whether or not 
    Player.new_deck is generating a full deck of 52 cards since we will not
    be using or looking at all 52 cards during the game. Furthermore, we
    might not realize if all the appropriate player values are changing at the
    right time, since the player might bust before this info is ever revealed
    while playing the actual game. In terms of generating these OUnit tests,
    we did a mix of black box tests s well as glass box which made us consider
    edge cases (ensuring an Ace is equal in value to 11 despite its value
    being 1)
    For the game and state modules, we tested them through play testing and 
    ensured that all functionality (hitting, standing, doublign down, splitting
    etc) is working properly. Further more, we ensured
    that money is getting moved appropriately since the updated
    money of each player is
    provided to us during the game at the end of each round. *)

let make_generate_cards_of_suit_test
    (name : string) 
    (st: Deck.suit) 
    (loop: int list)
    (acc : Deck.t)
    (expected_output : Deck.card list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (generate_cards_of_suit st loop acc))
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
    make_generate_cards_of_suit_test 
      "Diamond"  Diamond [1] [] [{suit = Diamond; value = 1}];
    make_generate_cards_of_suit_test 
      "King of Club"  Club [13] [] [{suit = Club; value = 13}];
    make_generate_cards_of_suit_test 
      "King of Heart"  Heart [13] [] [{suit = Heart; value = 13}];
    make_generate_cards_of_suit_test 
      "Three Cards"  Heart [1;2;3] [] [{suit = Heart; value = 3};
                                       {suit = Heart; value = 2};
                                       {suit = Heart; value = 1}];
    make_generate_cards_of_suit_test 
      "5 Cards"  Heart [1;2;3;4;5] [] [{suit = Heart; value = 5};
                                       {suit = Heart; value = 4};
                                       {suit = Heart; value = 3};
                                       {suit = Heart; value = 2};
                                       {suit = Heart; value = 1}];
    make_generate_cards_of_suit_test 
      "10 Cards"  Heart [1;2;3;4;5;6;7;8;9;10] [] [{suit = Heart; value = 10};
                                                   {suit = Heart; value = 9};
                                                   {suit = Heart; value = 8};
                                                   {suit = Heart; value = 7};
                                                   {suit = Heart; value = 6};
                                                   {suit = Heart; value = 5};
                                                   {suit = Heart; value = 4};
                                                   {suit = Heart; value = 3};
                                                   {suit = Heart; value = 2};
                                                   {suit = Heart; value = 1}];

  ]

(** [make_new_deck_test name] 
    constructs an OUnit test named [name] that checks whether 
    [new_deck] returns the same value
    as [expected_output] *)
let make_new_deck_test
    (name : string) 
    (expected_output : Deck.card list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Deck.new_deck))




let new_deck_test = [
  make_new_deck_test "New Deck" standard_deck;
]



(** [make_val_of_test name card] 
    constructs an OUnit test named [name] that checks whether 
    [val_of card] returns the same value
    as [expected_output] *)
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
  make_val_of_test
    "Assert 5 Value is 5" {suit = Diamond; value = 5} 5;
  make_val_of_test
    "Assert 10 Value is 10 with Spade" {suit = Spade; value = 10} 10;
  make_val_of_test
    "Assert Ace value 11 with spade suit" {suit = Spade; value = 1} 11;
  make_val_of_test
    "Assert Jack value 10 with spade suit" {suit = Spade; value = 10} 10;
  make_val_of_test
    "Assert Queen value 10 with spade suit" {suit = Spade; value = 12} 10;

]

(** [make_length_test deck] 
    constructs an OUnit test named [name] that checks whether 
    [length deck] returns the same value
    as [expected_output] *)
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
    make_length_test "length two deck" [{suit = Heart; value = 1}; 
                                        {suit = Diamond; value = 1}] 2;
    make_length_test "length five deck" [{suit = Heart; value = 1}; 
                                         {suit = Diamond; value = 1};
                                         {suit = Heart; value = 2}; 
                                         {suit = Diamond; value = 2};
                                         {suit = Diamond; value = 3};] 5;
    make_length_test "length 0 deck" [] 0;
    make_length_test "length 3 deck" [{suit = Heart; value = 1}; 
                                      {suit = Diamond; value = 1};
                                      {suit = Heart; value = 2}; ] 3;
    make_length_test "length 30 deck" [{suit = Heart; value = 1}; 
                                       {suit = Diamond; value = 1};
                                       {suit = Heart; value = 2}; 
                                       {suit = Diamond; value = 2};
                                       {suit = Diamond; value = 3};
                                       {suit = Heart; value = 1}; 
                                       {suit = Diamond; value = 1};
                                       {suit = Heart; value = 2}; 
                                       {suit = Diamond; value = 2};
                                       {suit = Diamond; value = 3};
                                       {suit = Heart; value = 1}; 
                                       {suit = Diamond; value = 1};
                                       {suit = Heart; value = 2}; 
                                       {suit = Diamond; value = 2};
                                       {suit = Diamond; value = 3};
                                       {suit = Heart; value = 1}; 
                                       {suit = Diamond; value = 1};
                                       {suit = Heart; value = 2}; 
                                       {suit = Diamond; value = 2};
                                       {suit = Diamond; value = 3};
                                       {suit = Heart; value = 1}; 
                                       {suit = Diamond; value = 1};
                                       {suit = Heart; value = 2}; 
                                       {suit = Diamond; value = 2};
                                       {suit = Diamond; value = 3};
                                       {suit = Heart; value = 1}; 
                                       {suit = Diamond; value = 1};
                                       {suit = Heart; value = 2}; 
                                       {suit = Diamond; value = 2};
                                       {suit = Diamond; value = 3};] 30;
  ]

(** [make_command_test name str] 
    constructs an OUnit test named [name] that checks whether 
    [parse str] returns the same value
    as [expected_output] *)
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
  {id = 1; hand = []; count = 0; cash = 200; split_hand = []; split_count = 0;}
let player_2 = 
  {id = 2; hand = []; count = 0; cash = 500; split_hand = []; split_count = 0;}

let player_3 = 
  {id = 3; hand = []; count = 10; cash = 700; split_hand = []; split_count = 0;}

let player_4 = 
  {id = 4; hand = []; count = 15; cash = 700; split_hand = []; split_count = 0;}

let new_player_test = 
  [
    make_new_player_test "Player 1 200 dollars" 1 200 player_1;
    make_new_player_test "Player 2 500 dollars" 2 500 player_2;
  ]

let make_get_id_test
    (name : string) 
    (player : Player.t) 
    (expected_output : player_id) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_id player))

let get_id_tests = 
  [
    make_get_id_test "Player 1 id" player_1 1;
    make_get_id_test "Player 2 id" player_2 2;
    make_get_id_test "Player 3 id" player_3 3;
    make_get_id_test "Player 4 id" player_4 4;
  ]

let make_get_count_test
    (name : string) 
    (player : Player.t) 
    (expected_output : player_count) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (get_count player))

let get_count_tests = 
  [
    make_get_count_test "Player 1 count is 0" player_1 0;
    make_get_count_test "Player 2 count is 0" player_2 0;
    make_get_count_test "Player 3 count is 10" player_3 10;
    make_get_count_test "Player 4 count is 15" player_4 15;
  ]

let make_set_cash_test
    (name : string) 
    (player : Player.t) 
    (money: int)
    (expected_output : Player.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (set_cash player money))

let player_1_updated = 
  {id = 1; hand = []; count = 0; cash = 500; split_hand = []; split_count = 0;}
let player_2_updated = 
  {id = 2; hand = []; count = 0; cash = 200; split_hand = []; split_count = 0;}
let player_3_updated = 
  {id = 3; hand = []; count = 10; cash = 0; split_hand = []; split_count = 0;}
let player_4_updated = 
  {id = 4; hand = []; count = 15; cash = 1000; split_hand = []; split_count = 0;}
let player_2_second = 
  {id = 2; hand = []; count = 0; cash = 100; split_hand = []; split_count = 0;}

let set_cash_tests = 
  [
    make_set_cash_test "Change player 1 cash" player_1 500 player_1_updated;
    make_set_cash_test "Change player 2 cash" player_2 200 player_2_updated;
    make_set_cash_test "Change player 3 cash" player_3 0 player_3_updated;
    make_set_cash_test "Change player 4 cash" player_4 1000 player_4_updated;
    make_set_cash_test "Change player 2_second cash" player_2 100 player_2_second;
  ]

let suite =
  "test suite for A2" >::: List.flatten [
    cards_of_suit_tests;
    new_deck_test;
    val_of_tests;
    length_tests;
    command_tests;
    new_player_test;
    get_id_tests;
    get_count_tests;
    set_cash_tests;

  ]

let _ = run_test_tt_main suite


