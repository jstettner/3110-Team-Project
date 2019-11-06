open Deck
open Player

type bet = int

type t = {
  players: (Player.t * bet) list;
  deck: Deck.t;
  house: card list
}

let initial_cash = 500

let rec generate_n_players (players : Player.t list) = function
  | 0 -> players
  | n -> generate_n_players (Player.new_player n initial_cash :: players) (n - 1)

let new_game (player_num : int) : t = 
  let players = generate_n_players [] player_num in
  let players_tup = List.map (fun x -> (x, 0)) players in
  let deck = Deck.new_deck |> Deck.shuffle in
  {
    players=players_tup;
    deck=deck;
    house=[];
  }

let set_bet (st : t) (zet : bet) (player : player_id) : t =
  {
    players= (
      List.map (fun (ply, bt) -> 
          if Player.get_id ply = player then (ply, zet) else (ply, bt)
        ) st.players
    );
    deck=st.deck;
    house=st.house;
  }

let get_bet (st : t) (player : player_id) : bet =
  let (_, b) = List.find 
      (fun (ply, bt) -> Player.get_id ply = player) 
      st.players in b

let hit_helper (player: Player.t) (c: card) : Player.t = 
  ((player |> Player.add_to_hand) c |> Player.inc_count) (Deck.val_of c)

(* pre: deck not empty *)
let hit (st: t) (player : player_id) : t =
  (* add to player hand *)
  match Deck.choose st.deck with
  | None -> failwith("failed precondition")
  | Some (new_card, new_deck) -> begin
      {
        players= (
          List.map (fun (ply, bt) -> 
              if Player.get_id ply = player then (hit_helper ply new_card, bt) else (ply, bt)
            ) st.players
        );
        deck=new_deck;
        house=st.house;
      }
    end
