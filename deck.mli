(** [suit] is the type of suit a playing card can belong to. *)
type suit =
  | Heart
  | Diamond
  | Spade
  | Club

(** [value] is the type of point value of a playing card. *)
type value = int

(** [card] is the type of playing card that holds card suit and value. *)
type card = {
  suit: suit; value: value
}

(** [t] is the type of deck. *)
type t = card list

(** [new_deck] is a standard playing card deck with 52 cards. *)
val new_deck : t

(** [shuffle old_deck] is the deck containing all cards from [old_deck] in 
    shuffled order. *)
val shuffle : t -> t

(** [shuffle_n deck n] is the deck containing all cards from [deck] shuffled
    [n] times. *)
val shuffle_n : t -> int -> t

(** [choose deck] is (card * t) option returning None if [deck] is empty or
    Some (card * t) with the first card in [deck] and the new deck without the 
    first card. *)
val choose : t -> (card * t) option 

(** [length deck] is the card count of [deck]. *)
val length : t -> int

(** [to_list deck] is the list of cards in [deck]. *)
val to_list : t -> card list

(** [val_of c] is the playing value of card [c] according to Blackjack rules. *)
val val_of : card -> int