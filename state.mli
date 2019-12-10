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

(** [initial_cash] is the starting amount of cash for each player *)
val initial_cash : int 

(** [symbol_of_suit s] is the string that prints the ASCII symbol matching 
    suit [s]. *)
val symbol_of_suit : suit -> string

(** [left_face_of_value] is the string that prints the appropriate number or
    face card letter corresponding to card value [v] with the alphanumeric
    character aligned left. *)
val left_face_of_value : int -> string 

(** [right_face_of_value] is the string that prints the appropriate number or
    face card letter corresponding to card value [v] with the alphanumeric 
    character aligned right. *)
val right_face_of_value : int -> string

(** [print_card st card i] prints line [i] of card [card] to terminal. *)
val print_card : t -> card -> int -> unit

(** [print_hidden_card x] prints a face down playing card to terminal. *)
val print_hidden_card : int -> unit 

(** [print_hand st hand i] prints line [i] of each card in [hand]. *)
val print_hand : t -> card list -> int -> unit 

(** [get_player st p_id] is player object corresponding to player_id [p_id] 
    with the player's current bet amount, whether they busted or not, and 
    whether they have a soft or hard hand. *)
val get_player : t -> player_id -> Player.t * bet * bool * bust * bust * hard * hard

(** [print st id] prints each card in the hand of the player corresponding to
    player_id [id]. *)
val print : t -> player_id -> unit 

(** [print_split_id st id] prints each card in the  split 
    hand of the player corresponding to player_id [id]. *)
val print_split : t -> player_id -> unit

(** [print_dealer st] prints each card in the dealer's hand to terminal. *)
val print_dealer : t -> unit 


(** [generate_n_players players] generates n players for a new 
    game of blackjack. *)
val generate_n_players : Player.t list -> int -> Player.t list

(** [new_game player_num shuffle_amt] initializes a new game of blackjack *)
val new_game : int -> int -> t

(** [reset_round st] resets the round of blackjack to initial state of each
    round*)
val reset_round : t -> t 

(** [change_money st p_id money] sets the player's money attribute to
    [money]*)
val change_money : t -> player_id -> player_money -> t 

(** [double_player st p_id] sets the player's money attribute to
    [money]*)
val double_player : t -> player_id -> t

(** [bust_player st p_id] busts the player with id [p_id]*)
val bust_player : t -> player_id -> t 

(** [split_bust_player st p_id] busts the split hand of the player with id 
    [p_id]*)
val split_bust_player : t -> player_id -> t

(** [set_bet st zet player] sets the bet of [player] to [zet]*)
val set_bet : t -> bet -> player_id -> t 

(** [get_bet st player] returns the bet amount of [player]*)
val get_bet : t ->  player_id -> bet 

(** [new_deck st] returns initializes a new deck in the current game*)
val new_deck : t -> t 

(**[hit player st] causes the player to request another
   card and adds it to the hand of player with id [player]
    If this results in a bust, the round will
   terminate *)
val hit : player_id -> t -> t

(** pre: deck not empty *)
(**[split_hit player_id st] splits the hand of player with id [player]  *)
val split_hit : player_id -> t -> t

(**[dealer_hit st] Causes the dealer to add another card to their hand
   when appropriate. If this results in a bust, the round will terminate   *)
val dealer_hit : t -> t


(** [make_dealer_hard st] returns a state where the dealer's hand is hard. *)
val make_dealer_hard : t -> t 

(** [make_player_hard st p_id] returns a state where the player's hand is hard. *)
val make_player_hard : t -> player_id -> t

(** [make_player_split_hard st p_id] returns a state where the player's split 
    hand is hard. *)
val make_player_split_hard : t -> player_id -> t

(** [split_player st p_id] returns a state where the player's hand is split. *)
val split_player : t -> player_id -> t 