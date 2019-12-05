open Deck
open Player

(** [bet] is the type of *)
type bet = int
type bust = bool
type hard = bool

(** [t] is the type of state. *)
type t = {
  players: (Player.t * bet * bust * hard) list;
  deck: Deck.t;
  house: (Player.t * bust * hard);
  turn: player_id;
}

val initial_cash : int 

val symbol_of_suit : suit -> string

val left_face_of_value : int -> string 

val right_face_of_value : int -> string

val print_card : t -> card -> int -> unit

val print_hidden_card : int -> unit 

val print_hand : t -> card list -> int -> unit 

val get_player : t -> player_id -> Player.t * bet * bust * hard

val print : t -> player_id -> unit 

val print_dealer : t -> unit 

val generate_n_players : Player.t list -> int -> Player.t list

val new_game : int -> int -> t

val reset_round : t -> t 

val change_money : t -> player_id -> player_money -> t 

val bust_player : t -> player_id -> t 

val set_bet : t -> bet -> player_id -> t 

val get_bet : t ->  player_id -> bet 

val new_deck : t -> t 

val hit : player_id -> t -> t

val dealer_hit : t -> t

val make_dealer_hard : t -> t 

val make_player_hard : t -> player_id -> t