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

val choose : t -> (card * t) option 