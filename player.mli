open Deck

type player_id = int

type t = player_id * (card list)

val new_player : player_id -> t

val draw : Deck.t -> t -> (Deck.t * t)

val has_in_hand : suit -> value -> t -> bool

val remove_from_hand : suit -> value -> t -> t

val get_hand : t -> card list
