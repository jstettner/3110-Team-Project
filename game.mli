open State
open Deck
open Player
open Command

val betting_phase : State.t -> player_id -> int -> State.t

val dealing_phase : State.t -> player_id -> int -> State.t

val playing_phase : State.t -> player_id -> int -> State.t 

val find_busted : State.t -> player_id list 

val find_not_busted : State.t -> player_id list 

val dealer_busted : State.t -> bool

val player_win : State.t -> player_id -> State.t 

val player_loss : State.t -> player_id -> State.t 

val players_all_win : State.t -> player_id list -> State.t 

val players_all_lose : State.t -> player_id list -> State.t 

val did_win : State.t -> player_id -> bool

val who_won : State.t -> player_id list -> player_id list -> player_id list

val who_lost : State.t -> player_id list -> player_id list -> player_id list

val moving_money_phase : State.t -> State.t

val game_body : State.t -> State.t 

