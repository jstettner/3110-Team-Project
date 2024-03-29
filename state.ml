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


let symbol_of_suit (s : suit) : string = 
  match s with 
  | Heart -> "    ♥    "
  | Diamond -> "    ♦    "
  | Spade -> "    ♠    "
  | Club -> "    ♣    "


let left_face_of_value (v : int) : string =
  if v = 11 then "J        "
  else if v = 12 then "Q        "
  else if v = 13 then "K        "
  else if v = 1 then "A        "
  else if v = 10 then "10       "
  else string_of_int v ^ "        "


let right_face_of_value (v : int) : string =
  if v = 11 then "        J"
  else if v = 12 then "        Q"
  else if v = 13 then "        K"
  else if v = 1 then "        A"
  else if v = 10 then "       10"
  else "        " ^ string_of_int v 


let print_card (st : t) (card : card) (i : int) : unit =
  if i = 0 then print_string "┌─────────┐"
  else if i = 1 then print_string ("│" ^ left_face_of_value card.value ^ "│")
  else if i = 4 then print_string ("│" ^ symbol_of_suit card.suit ^ "│")
  else if i = 7 then print_string ("│" ^ right_face_of_value card.value ^ "│")
  else if i = 8 then print_string "└─────────┘"
  else print_string "│         │"


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

let rec print_hand (st : t) (hand : card list) (i : int) : unit =
  ignore (List.map (fun x -> print_card st x i) hand); print_endline ""


let get_player (st : t) (p_id: player_id) : Player.t * bet * bool *bust * bust * hard * hard =
  let found = List.find (fun (x, _, _, _, _, _, _) -> x.id = p_id) st.players in
  found


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


let print_dealer (st : t) : unit =
  let (dealer, _, _) = st.house in
  let hand = get_hand dealer in
  match hand with
  | h :: t -> begin print_hidden_card 0; 
      for j = 0 to 8 do
        (print_hand st t j) done end
  | _ -> ()

let rec generate_n_players (players : Player.t list) = function
  | 0 -> players
  | n -> generate_n_players (Player.new_player n initial_cash :: players) 
           (n - 1)


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


let get_bet (st : t) (player : player_id) : bet =
  let (_, b, _, _, _, _, _) = List.find 
      (fun (ply, bt, _, _, _, _, _) -> Player.get_id ply = player) 
      st.players in b

(** [hit_helper] adds a card to the hand of [player] and adds its count to their 
    total count*)
let hit_helper (player: Player.t) (c: card) : Player.t = 
  ((player |> Player.add_to_hand) c |> Player.inc_count) (Deck.val_of c)

(** [split_hit_helper] adds a card to the hand of [player] and *)
let split_hit_helper (player: Player.t) (c: card) : Player.t = 
  ((player |> Player.add_to_split_hand) c |> Player.inc_split_count) (Deck.val_of c)

let new_deck (st : t) : t = 
  {
    players=st.players;
    deck=(Deck.shuffle new_deck);
    house=st.house;
    turn=st.turn;
  }


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