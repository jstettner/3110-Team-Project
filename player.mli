open Deck

type player_id = int
type player_count = int
type player_money = int

type t = {
  id: player_id;
  hand: card list;
  count: player_count;
  cash: player_money
}

val new_player : player_id -> player_money -> t

val get_id : t -> player_id

val get_count : t -> player_count

val get_cash : t -> player_money

val get_hand : t -> card list

val new_round : t -> t

val add_to_hand : t -> card -> t

val inc_count : t -> int -> t
