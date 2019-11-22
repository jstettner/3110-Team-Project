open Deck
open Player

type bet = int
type bust = bool

type t = {
  players: (Player.t * bet * bust) list;
  deck: Deck.t;
  house: (Player.t * bust);
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

let get_player (st : t) (p_id: player_id) : Player.t * bet * bust =
  let found = List.find (fun (x, _, _) -> x.id = p_id) st.players in
  found

let print (st : t) (id: player_id) : unit =
  let (player, _, _) = get_player st id in
  let hand = get_hand player in
  for j = 0 to 8 do
    (print_hand st hand j) done;;

let print_dealer (st : t) : unit =
  let (dealer, _) = st.house in
  let hand = get_hand dealer in
  (* ignore (List.map (fun x -> print_endline ((string_of_int x.value))) hand); *)
  match hand with
  | h :: t -> begin print_hidden_card 0; 
      for j = 0 to 8 do
        (print_hand st t j) done end
  | _ -> ()

let rec generate_n_players (players : Player.t list) = function
  | 0 -> players
  | n -> generate_n_players (Player.new_player n initial_cash :: players) (n - 1)

let new_game (player_num : int) (shuffle_amt : int) : t = 
  let players = generate_n_players [] player_num in
  let players_tup = List.map (fun x -> (x, 0, false)) players in
  let deck = Deck.shuffle_n Deck.new_deck shuffle_amt in
  {
    players=players_tup;
    deck=deck;
    house=(new_player 0 0, false);
    turn=0;
  }

let bust_player (st : t) (p_id: player_id) : t =
  if p_id = 0 then
    let (dealer, _) = st.house in 
    {
      players= st.players;
      deck=st.deck;
      house=(dealer, true);
      turn=st.turn;
    }
  else
    {
      players= (
        List.map (fun (ply, bt, bust) -> 
            if Player.get_id ply = p_id then (ply, bt, true) else (ply, bt, bust)
          ) st.players
      );
      deck=st.deck;
      house=st.house;
      turn=st.turn;
    }


let set_bet (st : t) (zet : bet) (player : player_id) : t =
  {
    players= (
      List.map (fun (ply, bt, bust) -> 
          if Player.get_id ply = player then (ply, zet, bust) else (ply, bt, bust)
        ) st.players
    );
    deck=st.deck;
    house=st.house;
    turn=st.turn;
  }

let get_bet (st : t) (player : player_id) : bet =
  let (_, b, _) = List.find 
      (fun (ply, bt, _) -> Player.get_id ply = player) 
      st.players in b

let hit_helper (player: Player.t) (c: card) : Player.t = 
  ((player |> Player.add_to_hand) c |> Player.inc_count) (Deck.val_of c)

(* pre: deck not empty *)
let hit (player : player_id) (st: t) : t =
  (* add to player hand *)
  match Deck.choose st.deck with
  | None -> failwith("failed precondition")
  | Some (new_card, new_deck) -> begin
      let st' = 
        {
          players= (
            List.map (fun (ply, bt, bust) -> 
                if Player.get_id ply = player then (hit_helper ply new_card, bt, bust) else (ply, bt, bust)
              ) st.players
          );
          deck=new_deck;
          house=st.house;
          turn=st.turn;
        } in 
      (* print st' player;  *)
      st'
    end

let dealer_hit (st: t) : t =
  match Deck.choose st.deck with
  | None -> failwith("failed precondition")
  | Some (new_card, new_deck) -> begin
      let (dealer, bust) = st.house in 
      let st' = 
        {
          players=st.players;
          deck=new_deck;
          house=(hit_helper dealer new_card, bust);
          turn=st.turn;
        } in 
      (* print_dealer st';  *)
      st'
    end