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

let rec generate_n_players (players : Player.t list) = function
  | 0 -> players
  | n -> generate_n_players (Player.new_player n initial_cash :: players) (n - 1)

let new_game (player_num : int) : t = 
  let players = generate_n_players [] player_num in
  let players_tup = List.map (fun x -> (x, 0, false)) players in
  let deck = Deck.new_deck |> Deck.shuffle in
  {
    players=players_tup;
    deck=deck;
    house=(new_player 0 0, false);
    turn=0;
  }

let get_player (st : t) (p_id: player_id) : Player.t =
  let players = List.map (fun (p, _, _) -> p) st.players in
  List.find (fun x -> x.id = p_id) players

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
      {
        players= (
          List.map (fun (ply, bt, bust) -> 
              if Player.get_id ply = player then (hit_helper ply new_card, bt, bust) else (ply, bt, bust)
            ) st.players
        );
        deck=new_deck;
        house=st.house;
        turn=st.turn;
      }
    end

let dealer_hit (st: t) : t =
  match Deck.choose st.deck with
  | None -> failwith("failed precondition")
  | Some (new_card, new_deck) -> begin
      let (dealer, bust) = st.house in 
      {
        players=st.players;
        deck=new_deck;
        house=(hit_helper dealer new_card, bust);
        turn=st.turn;
      }
    end