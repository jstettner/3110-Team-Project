open Deck
open Player

(** [bet] is the type of *)
type bet = int
type bust = bool
type hard = bool

(** [t] is the type of state. *)
type t = {
  players: (Player.t * bet * bool * bust * bust * hard * hard) list;
  deck: Deck.t;
  house: (Player.t * bust * hard);
  turn: player_id;
}

let initial_cash = 500

(** [symbol_of_suit s] is the string that prints the ASCII symbol matching 
    suit [s]. *)
let symbol_of_suit (s : suit) : string = 
  match s with 
  | Heart -> "    ♥    "
  | Diamond -> "    ♦    "
  | Spade -> "    ♠    "
  | Club -> "    ♣    "

(** [left_face_of_value] is the string that prints the appropriate number or
    face card letter corresponding to card value [v] with the alphanumeric
    character aligned left. *)
let left_face_of_value (v : int) : string =
  if v = 11 then "J        "
  else if v = 12 then "Q        "
  else if v = 13 then "K        "
  else if v = 1 then "A        "
  else if v = 10 then "10       "
  else string_of_int v ^ "        "

(** [right_face_of_value] is the string that prints the appropriate number or
    face card letter corresponding to card value [v] with the alphanumeric 
    character aligned right. *)
let right_face_of_value (v : int) : string =
  if v = 11 then "        J"
  else if v = 12 then "        Q"
  else if v = 13 then "        K"
  else if v = 1 then "        A"
  else if v = 10 then "       10"
  else "        " ^ string_of_int v 

(** [print_card st card i] prints line [i] of card [card] to terminal. *)
let print_card (st : t) (card : card) (i : int) : unit =
  if i = 0 then print_string "┌─────────┐"
  else if i = 1 then print_string ("│" ^ left_face_of_value card.value ^ "│")
  else if i = 4 then print_string ("│" ^ symbol_of_suit card.suit ^ "│")
  else if i = 7 then print_string ("│" ^ right_face_of_value card.value ^ "│")
  else if i = 8 then print_string "└─────────┘"
  else print_string "│         │"

(** [print_hidden_card x] prints a face down playing card to terminal. *)
let print_hidden_card x: unit =
  begin 
    print_endline "┌─────────┐";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "│░░░░░░░░░│";
    print_endline "└─────────┘";
  end

(** [print_hand st hand i] prints line [i] of each card in [hand]. *)
let rec print_hand (st : t) (hand : card list) (i : int) : unit =
  ignore (List.map (fun x -> print_card st x i) hand); print_endline ""

(** [get_player st p_id] is player object corresponding to player_id [p_id] 
    with the player's current bet amount, whether they busted or not, and 
    whether they have a soft or hard hand. *)
let get_player (st : t) (p_id: player_id) : Player.t * bet * bool *bust * bust * hard * hard =
  let found = List.find (fun (x, _, _, _, _, _, _) -> x.id = p_id) st.players in
  found

(** [print st id] prints each card in the hand of the player corresponding to
    player_id [id]. *)
let print (st : t) (id: player_id) : unit =
  let (player, _, _, _, _, _, _) = get_player st id in
  let hand = get_hand player in
  for j = 0 to 8 do
    (print_hand st hand j) done;;

let print_split (st : t) (id: player_id) : unit =
  let (player, _, _, _, _, _, _) = get_player st id in
  let hand = get_split_hand player in
  for j = 0 to 8 do
    (print_hand st hand j) done;;

(** [print_dealer st] prints each card in the dealer's hand to terminal. *)
let print_dealer (st : t) : unit =
  let (dealer, _, _) = st.house in
  let hand = get_hand dealer in
  match hand with
  | h :: t -> begin print_hidden_card 0; 
      for j = 0 to 8 do
        (print_hand st t j) done end
  | _ -> ()

(** [generate n players players] generates n players for a new 
    game of blackjack. *)
let rec generate_n_players (players : Player.t list) = function
  | 0 -> players
  | n -> generate_n_players (Player.new_player n initial_cash :: players) 
           (n - 1)

(** [new_game player_num shuffle_amt] initializes a new game of blackjack *)
let new_game (player_num : int) (shuffle_amt : int) : t = 
  let players = generate_n_players [] player_num in
  let players_tup = List.map (fun x -> (x, 0, false, false, false, true, true)) players in
  let deck = Deck.shuffle_n Deck.new_deck shuffle_amt in
  {
    players=players_tup;
    deck=deck;
    house=(new_player 0 0, false, true);
    turn=0;
  }

(** [reset_round st] resets the round of blackjack to initial state of each
    round*)
let reset_round (st : t) : t =
  let players' = List.map (fun (p, _, _, _, _, _, _) -> (new_round p, 0, false, false, false, true, true)) 
      st.players in
  let (house, _, _) = st.house in 
  let house' = (new_round house, false, true) in
  {
    players=players';
    deck=st.deck;
    house=house';
    turn=0;
  }

(** [change_money st p_id money] sets the player's money attribute to
    [money]*)
let change_money (st : t) (p_id : player_id) (money : player_money) : t =
  {
    players= (
      List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
          if Player.get_id ply = p_id then (set_cash ply money, bt, doub, bust, split_bust, hard, split_hard) 
          else (ply, bt, doub, bust, split_bust, hard, split_hard)
        ) st.players
    );
    deck=st.deck;
    house=st.house;
    turn=st.turn;
  }

(** [double_player st p_id] sets the player's money attribute to
    [money]*)
let double_player (st : t) (p_id: player_id) : t =
  {
    players= (
      List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
          if Player.get_id ply = p_id then (ply, bt, true, bust, split_bust, hard, split_hard) else 
            (ply, bt, doub, bust, split_bust, hard, split_hard)
        ) st.players
    );
    deck=st.deck;
    house=st.house;
    turn=st.turn;
  }

(** [bust_player st p_id] busts the player with id [p_id]*)
let bust_player (st : t) (p_id: player_id) : t =
  if p_id = 0 then
    let (dealer, _, _) = st.house in 
    {
      players= st.players;
      deck=st.deck;
      house=(dealer, true, false);
      turn=st.turn;
    }
  else
    {
      players= (
        List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
            if Player.get_id ply = p_id then (ply, bt, doub, true, split_bust, hard, split_hard) else 
              (ply, bt, doub, bust, split_bust, hard, split_hard)
          ) st.players
      );
      deck=st.deck;
      house=st.house;
      turn=st.turn;
    }

(** [split_bust_player st p_id] busts the split hand of the player with id 
    [p_id]*)
let split_bust_player (st : t) (p_id: player_id) : t =
  {
    players= (
      List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
          if Player.get_id ply = p_id then (ply, bt, doub, bust, true, hard, split_hard) else 
            (ply, bt, doub, bust, split_bust, hard, split_hard)
        ) st.players
    );
    deck=st.deck;
    house=st.house;
    turn=st.turn;
  }

(** [set_bet st zet player] sets the bet of [player] to [zet]*)
let set_bet (st : t) (zet : bet) (player : player_id) : t =
  {
    players= (
      List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
          if Player.get_id ply = player then (ply, zet, doub, bust, split_bust, hard, split_hard) else 
            (ply, bt, doub, bust, split_bust, hard, split_hard)
        ) st.players
    );
    deck=st.deck;
    house=st.house;
    turn=st.turn;
  }

(** [get_bet st player] returns the bet amount of [player]*)
let get_bet (st : t) (player : player_id) : bet =
  let (_, b, _, _, _, _, _) = List.find 
      (fun (ply, bt, _, _, _, _, _) -> Player.get_id ply = player) 
      st.players in b

let hit_helper (player: Player.t) (c: card) : Player.t = 
  ((player |> Player.add_to_hand) c |> Player.inc_count) (Deck.val_of c)

let split_hit_helper (player: Player.t) (c: card) : Player.t = 
  ((player |> Player.add_to_split_hand) c |> Player.inc_split_count) (Deck.val_of c)

let new_deck (st : t) : t = 
  {
    players=st.players;
    deck=(Deck.shuffle new_deck);
    house=st.house;
    turn=st.turn;
  }

(* pre: deck not empty *)
let rec split_hit (player : player_id) (st: t) : t =
  (* add to player hand *)
  match Deck.choose st.deck with
  | None -> split_hit player (new_deck st)
  | Some (new_card, new_deck) -> begin
      if (new_card.value = 1) then 
        {
          players= (
            List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
                if Player.get_id ply = player then 
                  (split_hit_helper ply new_card, bt, doub, bust, split_bust, false, split_hard) else 
                  (ply, bt, doub, bust, split_bust, hard, split_hard)
              ) st.players
          );
          deck=new_deck;
          house=st.house;
          turn=st.turn;
        }
      else 
        {
          players= (
            List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
                if Player.get_id ply = player then 
                  (split_hit_helper ply new_card, bt, doub, bust, split_bust, hard, split_hard) else 
                  (ply, bt, doub, bust, split_bust, hard, split_hard)
              ) st.players
          );
          deck=new_deck;
          house=st.house;
          turn=st.turn;
        }
    end

(* pre: deck not empty *)
let rec hit (player : player_id) (st: t) : t =
  (* add to player hand *)
  match Deck.choose st.deck with
  | None -> hit player (new_deck st)
  | Some (new_card, new_deck) -> begin
      if (new_card.value = 1) then 
        {
          players= (
            List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
                if Player.get_id ply = player then 
                  (hit_helper ply new_card, bt, doub, bust, split_bust, false, split_hard) else 
                  (ply, bt, doub, bust, split_bust, hard, split_hard)
              ) st.players
          );
          deck=new_deck;
          house=st.house;
          turn=st.turn;
        }
      else 
        {
          players= (
            List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
                if Player.get_id ply = player then 
                  (hit_helper ply new_card, bt, doub, bust, split_bust, hard, split_hard) else 
                  (ply, bt, doub, bust, split_bust, hard, split_hard)
              ) st.players
          );
          deck=new_deck;
          house=st.house;
          turn=st.turn;
        }
    end

let rec dealer_hit (st: t) : t =
  match Deck.choose st.deck with
  | None -> dealer_hit (new_deck st)
  | Some (new_card, new_deck) -> begin
      let (dealer, bust, hard) = st.house in
      if (new_card.value = 1) then 
        {
          players=st.players;
          deck=new_deck;
          house=(hit_helper dealer new_card, bust, false);
          turn=st.turn;
        } 
      else 
        {
          players=st.players;
          deck=new_deck;
          house=(hit_helper dealer new_card, bust, hard);
          turn=st.turn;
        }
    end

let make_dealer_hard (st : t) : t =
  let (dealer, bust, hard) = st.house in 
  {
    players=st.players;
    deck=st.deck;
    house=(Player.sub_count_ten dealer, bust, true);
    turn=st.turn;
  }

let make_player_hard (st : t) (p_id : player_id) : t =
  {
    players= (
      List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
          if Player.get_id ply = p_id then 
            (Player.sub_count_ten ply, bt, doub, bust, split_bust, true, split_hard) else 
            (ply, bt, doub, bust, split_bust, hard, split_hard)
        ) st.players
    );
    deck=st.deck;
    house=st.house;
    turn=st.turn;
  }

let make_player_split_hard (st : t) (p_id : player_id) : t =
  {
    players= (
      List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
          if Player.get_id ply = p_id then 
            (Player.sub_count_ten ply, bt, doub, bust, split_bust, hard, true) else 
            (ply, bt, doub, bust, split_bust, hard, split_hard)
        ) st.players
    );
    deck=st.deck;
    house=st.house;
    turn=st.turn;
  }

let split_player (st : t) (p_id : player_id) : t =
  {
    players= (
      List.map (fun (ply, bt, doub, bust, split_bust, hard, split_hard) -> 
          if Player.get_id ply = p_id then 
            (Player.split_hand ply, bt, doub, bust, split_bust, hard, split_hard) else 
            (ply, bt, doub, bust, split_bust, hard, split_hard)
        ) st.players
    );
    deck=st.deck;
    house=st.house;
    turn=st.turn;
  }