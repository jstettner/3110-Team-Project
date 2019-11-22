type suit =
  | Heart
  | Diamond
  | Spade
  | Club

type value = int

type card = {
  suit: suit; value: value
}

type t = card list

val new_deck : t

val shuffle : t -> t

val shuffle_n : t -> int -> t

val choose : t -> (card * t) option 

val length : t -> int

val to_list : t -> card list

val val_of : card -> int