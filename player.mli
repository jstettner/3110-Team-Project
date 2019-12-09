open Deck

(** [player_id] is each player's unique integer determining playing order. *)
type player_id = int

(** [player_count] is each player's current hand total. *)
type player_count = int

(** [player_money] is each player's current cash. *)
type player_money = int

(** [t] is the type of player. *)
type t = {
  id: player_id;
  hand: card list;
  count: player_count;
  cash: player_money;
  split_hand: card list;
  split_count: player_count;
}

(** [new_player id starting_cash] is a new player with player_id [id]
    and player_money [starting_cash]. *)
val new_player : player_id -> player_money -> t

(** [get_id p] is the unique integer assigned to player [p]. *)
val get_id : t -> player_id

(** [get_count p] is the hand total of the player [p]. *)
val get_count : t -> player_count

(** [get_split_count p] is the split hand total of the player [p]. *)
val get_split_count : t -> player_count

(** [get_cash p] is the cash of player [p]. *)
val get_cash : t -> player_money

(** [get_hand p] is the hand of player [p]. *)
val get_hand : t -> card list

(** [get_split_hand p] is the split hand of player [p]. *)
val get_split_hand : t -> card list 

(** [sub_count_ten p] is the player identical to player [p] with count
    lowered by 10. *)
val sub_count_ten : t -> t

(** [split_sub_count_ten p] is the player identical to player [p] with split_count
    lowered by 10. *)
val split_sub_count_ten : t -> t

(** [set_cash p money] is the player identical to player [p] with player_money
    [money]. *)
val set_cash : t -> player_money -> t

(** [new_round p] is the player identical to player [p] with hand reset and 
    player_count [0]. *)
val new_round : t -> t

(** [add_to_hand p c] is the player identical to player [p] with card [c] added
    to the player hand. *)
val add_to_hand : t -> card -> t

(** [add_to_split_hand p c] is the player identical to player [p] with card [c] added
    to the player's split hand. *)
val add_to_split_hand : t -> card -> t

(** [inc_count p count] is the player identical to player [p] with count
    increased by [count]. *)
val inc_count : t -> int -> t

(** [inc_split_count p count] is the player identical to player [p] with split_count
    increased by [count]. *)
val inc_split_count : t -> int -> t

(**  [split_hand p] is the player p altered to have one of the two cards
     in hand moved into the [split_hand], with the counts updated respectively. *)
val split_hand : t -> t